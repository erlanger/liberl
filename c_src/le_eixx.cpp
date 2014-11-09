#include "le/le_eixx.hpp"

int main()
{
   using namespace eixx;
   //Simulate receiving binary from erlang
   eterm t0 = eterm::format("{ ok, 30,\"hello, I am ok\",atom1,40.1,50}");
   auto t0ext = t0.encode(0);

   std::cerr << "eloop=" << (le::exit_loop() == true) << std::endl;
   //le::send_term(t0);
   
   //Decode binary and store in local variables
   //and binding
   auto et = eterm(t0ext.c_str(),t0ext.size());
   auto pat = eterm::format("{ok, 30, S, atom1, F, I}");
   auto pat1 = eterm(var(atom("_")));
   auto d = le::make_dispatcher(
         pat, [=] (varbind& vb) {return pat.apply(vb);},
         "{S}", [=] (varbind& vb) {return le::fmt("{ok, S}",vb); },
         le::anyterm(), [=] (varbind& vb) { std::cerr << "got null!\n"; return le::nullterm(); }
         );
   d(t0);
   d(eterm::format("{hello}"));
   d("ok");
   d(5);
   varbind b;
   std::cerr << "t=" << et.match(pat,&b) << std::endl;
   std::cerr << "v1=" << b["S"]->to_string() << b["F"]->to_double();

   auto t1 = et.to_tuple();
   int value1 = t1[1].to_long(); 
   std::string value2 = t1[2].to_str().c_str();
   std::cerr << "comparing atom ok==ok: " << bool(t1[0].to_atom()=="ok")  << std::endl;
   std::cerr << "value1=" << value1 << std::endl;
   std::cerr << "value2=" << value2 << std::endl;
   std::cerr << t0.to_string() << "==" <<  et.to_string() << std::endl;

   //Encode term and send to erlang
   tuple tup{atom("ok"),value1+20,10,"It worked"};
   eterm t(tup);
   std::cerr << "response term = " << t.to_string() << std::endl;
   std::cerr << "encoded:" << std::endl;
   //print_buf(t.encode(0)) << std::endl;
   //
   le::enter_loop();

}
