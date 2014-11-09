#ifndef _LE_EIXX_HPP_
#define _LE_EIXX_HPP_

#include <cstdio>
#include <stdexcept>

#include "eixx/alloc_std.hpp"
#include "eixx/eterm.hpp"

#define LIBERL_NAMESPACE le

namespace LIBERL_NAMESPACE {
   using namespace eixx;
   enum exit_t {USER,STDOUT_CLOSED};
   namespace impl {
      enum exit_loop_t {QUERY,YES};
      void read(char* buf,std::streamsize len);
      const eterm dispatch(const eterm& pattern,
                           const eterm& value,
                           std::function<const eterm(varbind&)> f);
   }

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

inline std::function<eterm(const eterm&)> make_dispatcher()
{ return [=] (const eterm& value) { return nullterm(); }; }

inline std::function<eterm(const eterm&)> make_dispatcher(eterm nullterm(),
      std::function<const eterm(varbind&)> f)
{ return make_dispatcher(); }

template<typename ...Args>
 std::function<eterm(const eterm&)> 
 make_dispatcher(eterm pat,
      std::function<const eterm(varbind&)> f,
      Args... args);

template<typename ...Args>
 std::function<eterm(const eterm&)> 
 make_dispatcher(const char* s,
      std::function<const eterm(varbind&)> f,
      Args... args)
{
   eterm pat(eterm::format(s));
   return make_dispatcher(pat,f,args...);
}

template<typename ...Args>
 std::function<eterm(const eterm&)> 
 make_dispatcher(eterm pat,
      std::function<const eterm(varbind&)> f,
      Args... args)
{
   return [=] (const eterm& value) {
      eterm result = impl::dispatch(pat,value,f);
      if (!result.empty())
         return result;
      else {
         auto d=make_dispatcher(args...);
         return d(value);
      }
   };
}


inline std::string encode(const eterm& term)
{
   string s=term.encode(hdr_size(),true);
   std::string sout(s.c_str(),s.size());
   std::cerr << term.to_string() << "=";
   std::cerr << to_binary_string(sout.c_str(),s.size()) << std::endl;
   return sout;
}

inline std::size_t encode_size(const eterm& term)
{ return term.encode_size(hdr_size(),true); }

inline unsigned int to_number(const char c)
{ return static_cast<unsigned int>(static_cast<unsigned char>(c)); }

inline void send_term(const eterm& term)
{
   if (!term.empty()) {
      std::string s = encode(term);
      // std::cout.write(s.c_str(),s.size());
   }
}

inline eterm fmt(const char* s)
{ return eterm::format(s); }

inline eterm fmt(const char* s,varbind& vb)
{ return fmt(s).apply(vb); }

// Called by user to signal loop exit
inline bool exit_loop(const impl::exit_loop_t action=impl::YES) {
   static bool continue_loop = true;
   return (action==impl::QUERY) ? continue_loop:(continue_loop=false);
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

//Enter event loop
inline const exit_t enter_loop()//const DispatchTable& dt)
{
   static std::vector<char> buf;

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

   while(!exit_loop(impl::QUERY))
   {
      //Read size header and reserve space in buffer
      uint64_t sz = get_packet_size();
      if (buf.capacity() < sz)
         buf.reserve(sz);
      
      //Read sz bytes of data and decode the term
      impl::read(buf.data(),sz);
      eterm eterm(buf.data(),sz);
      #ifdef LE_DBG
         std::cerr << "Received: " << eterm.to_string() << std::endl;
      #endif
   }
   return USER;
}

   namespace impl {
      using namespace eixx;
      inline void read(char* buf,std::streamsize len)
      {
         try { 
            std::cin.read(buf,len);
         }
         catch (std::ios_base::failure e) {
            std::stringstream s;
            s<<"Unable to read " << len << " chars from cin.";
            throw std::ios_base::failure(s.str());
         }
      }

      inline const eterm dispatch(const eterm& pattern,
                                   const eterm& value,
                                   std::function<const eterm(varbind&)> f)
      {
         varbind vb;
         if (!value.empty() && value.match(pattern,&vb)) {
            eterm et(f(vb));
            le::send_term(et);
            return et;
         } else {
            return nullterm();
         }
      }
   }

}
#endif
