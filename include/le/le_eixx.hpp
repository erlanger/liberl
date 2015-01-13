#ifndef _LE_EIXX_HPP_
#define _LE_EIXX_HPP_

#include <cstdio>
#include <stdexcept>

#include "eixx/alloc_std.hpp"
#include "eixx/eterm.hpp"

#define LIBERL_NAMESPACE le

namespace LIBERL_NAMESPACE {
   using namespace eixx;
   namespace impl {
      enum  exit_loop_t {QUERY,YES};
      void  read(char* buf,std::streamsize len);
      eterm get_pattern(const eterm& pat);
      eterm get_pattern(const char* s);
      bool  input_available();
      bool check_stop_term(const eterm& term_in,
            std::function<eterm(const eterm&)> stop_fun);
      uint64_t get_packet_size();
   }

   struct eof:public std::ios_base::failure
   {
      eof(const std::string& what)
         :std::ios_base::failure(what)
      {}
   };

// API
   inline const eterm& nullterm()
   {
      static const eterm nullterm;
      return nullterm;
   }

   inline const eterm& anyterm()
   {
      static const eterm anyterm(var(atom("_")));
      return anyterm;
   }

   inline unsigned char hdr_size(unsigned char new_size=255) {
      //default header size (for byte count) is 4 bytes
      static unsigned char header_size = 4;
      if (new_size==4 || new_size==2 || new_size==1 || new_size==0)
         header_size=new_size;
      else if (new_size!=255)
         throw(std::invalid_argument("Invalid header size: only 0,1,2, or 4 allowed"));

      return header_size;
   }


   inline std::string encode(const eterm& term)
   {
      string s=term.encode(hdr_size(),true);
      std::string sout(s.c_str(),s.size());
      return sout;
   }

   inline bool prepare_streams()
   {
      //Make sure cin/cout throw exceptions on failure
      std::cout.exceptions(std::ios_base::failbit | std::ios_base::badbit);
      std::cin.exceptions(std::ios_base::failbit | std::ios_base::badbit);

      //Make sure C++ flushes its buffer
      //immediately on insertion
      std::cout.setf(std::ios::unitbuf);
      std::ios::sync_with_stdio(true); //mostly here for documentation
      //this is the default

      //Make sure stdin/stdout are not
      //buffered (hopefully the OS will honor this)
      setbuf(stdin,0);
      setbuf(stdout,0);
      return true;
   }

   inline void send_term(const eterm& term)
   {
      static bool prep_stdio = prepare_streams(); //Initialize streams on first call

      (void) prep_stdio; //Ignore unused-var warning

      if (!term.empty()) {
         std::string s = encode(term);
         std::cout.write(s.c_str(),s.size());
      }
   }

   inline eterm read_term(bool block=true)
   {
      static bool prep_stdio = prepare_streams(); //Initialize streams on first call
      static std::vector<char> buf;

      (void) prep_stdio; //Ignore unused-var warning

      //Return a nullterm if we are in nonblocking mode and there is no input waiting
      if (!block && !impl::input_available()) return nullterm();

      //Read size header and reserve space in buffer
      uint64_t sz = impl::get_packet_size();
      if (buf.capacity() < sz)
         buf.reserve(sz);

      //Read sz bytes of data and decode the term
      impl::read(buf.data(),sz);
      return eterm(buf.data(),sz);
   }

   std::function<eterm(const eterm&)> make_dispatcher()
   { return [] (const eterm& value) { return nullterm(); }; }

   template<typename T,typename ...Args>
      std::function<eterm(const eterm&)>
      make_dispatcher(T pat,
            std::function<const eterm(varbind&)> f,
            Args...args )
      {
         return [=] (const eterm& value) {
            varbind vb;
            if (value.match(impl::get_pattern(pat),&vb)){
               eterm result = f(vb);
               send_term(result);
               return result;
            } else {
               //This should not use many resources
               //it's depth is only half the
               //size of the original argument list
               auto d=make_dispatcher(args...);
               return d(value);
            }
         };
      }

   inline std::size_t encode_size(const eterm& term)
   { return term.encode_size(hdr_size(),true); }

   template<typename ...Args>
      inline void send(const char* s,Args...args)
      { send_term(eterm::format(s,args...)); }

   template<typename ...Args>
      inline eterm fmt(const char* s,Args...args)
      { return eterm::format(s,args...); }

   template<typename ...Args>
      inline eterm fmt(varbind& vb,const char* s,Args...args)
      { return fmt(s,args...).apply(vb); }


   inline bool check_stop(std::function<eterm(const eterm&)> stop_fun=
         [] (const eterm& reason) { (void) reason; return nullterm(); } )
   {
      if (impl::input_available()) {
         varbind vb;
         eterm term_in=read_term();
         // check_stop_term will send to erlang the term returned
         // by stop_fun, if it is not a nullterm()
         return impl::check_stop_term(term_in,stop_fun);
      } else {
         //No input available
         return false;
      }
   }

   // Called by user to signal loop exit
   inline bool exit_loop(const impl::exit_loop_t action=impl::YES) {
      static bool exit_loop = false;
      return (action==impl::QUERY) ? exit_loop:(exit_loop=true);
   }

   inline bool dispatch(std::function<eterm(const eterm&) > term_dispatcher)
   {
      eterm in_term=read_term(false);
      if (!in_term.empty()) {
         eterm out_term = term_dispatcher(in_term);
         return true;
      } else
         return false;
   }

   inline void wait_and_dispatch(std::function<eterm(const eterm&) > term_dispatcher,std::function<eterm(const eterm&)> stop_fun)
   {

      eterm in_term=read_term();

      //Dispatch the term to the appropriate handlers
      //defined by the user
      eterm out_term = term_dispatcher(in_term);
      if (stop_fun!=nullptr && impl::check_stop_term(in_term,stop_fun))
      {
         le::exit_loop();
         std::cerr << "Erlang stopped me." << std::endl;
      }

#ifdef LE_DBG
      std::cerr << "Received: " << in_term.to_string() << std::endl;
      std::cerr << "Sent:     " << (!out_term.empty() ? out_term.to_string():"--nothing--") << std::endl;
#endif
   }

   //Enter event loop
   template<typename Fb,typename Fa>
      inline void enter_loop( std::function<eterm(const eterm&)> term_dispatcher,
            std::function<eterm(const eterm&)> stop_fun,
            Fb before,
            Fa after)
      throw(eof)
      {
         prepare_streams();
         while(!exit_loop(impl::QUERY))
         {
            before();
            wait_and_dispatch(term_dispatcher, stop_fun);
            after();
         }
      }

   inline void enter_loop( std::function<eterm(const eterm&)> term_dispatcher,
         std::function<eterm(const eterm&)> stop_fun=
         [] (const eterm& reason) { (void) reason; return nullterm(); } )
   { enter_loop(term_dispatcher, stop_fun,  [] () {},    [] () {} ); }


   namespace impl {
      using namespace eixx;
      inline void read(char* buf,std::streamsize len)
      {
         try {
            std::cin.read(buf,len);
         }
         catch (std::ios_base::failure e) {
            std::stringstream s;
            if (std::cin.eof()) {
               //Erlang closed stdin, we need to exit
               s<<"cin was closed, erlang requires ports to terminate when stdin is closed";
               throw eof(s.str());
            } else {
               s<<"Unable to read " << len << " chars from cin.";
               throw std::ios_base::failure(s.str());
            }
         }
      }

      inline eterm get_pattern(const eterm& pat)
      { return pat; };

      inline eterm get_pattern(const char* s)
      { return eterm::format(s); };

      // returns true if there is input waiting in stdin, otherwise false
      inline bool input_available()
      {
         struct timeval tv;
         fd_set fds;
         tv.tv_sec = 0;
         tv.tv_usec = 0;
         FD_ZERO(&fds);
         FD_SET(STDIN_FILENO, &fds);
         select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
         return (FD_ISSET(0, &fds));
      }

      inline bool check_stop_term(const eterm& term_in,
            std::function<eterm(const eterm&)> stop_fun)
      {
         static eterm stop_term=eterm::format("{stop, Reason}");
         varbind vb;
         // Match against {stop, Reason}
         if (term_in.match(stop_term,&vb)) {
            const eterm reason = *(vb["Reason"]);
            const eterm result = stop_fun(reason);
            send_term(result);
            return true;
         } else {
            //No match
            return false;
         }
      }

   inline uint64_t get_packet_size() {
      char buf[4];
      const char* bufp=buf;

      impl::read(buf,hdr_size());
      switch(hdr_size()) {
         case 1:
            return get8(bufp);
         case 2:
            return get16be(bufp);
         case 4:
            return get32be(bufp);
         default:
            throw(std::invalid_argument("Invalid header size, only 1,2,4 are accepted for reading"));
      }
   }

   }

}
#endif
