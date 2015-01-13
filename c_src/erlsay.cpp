#include "le/le_eixx.hpp"

//This program is meant to be used to test the gen_exe server (erlang side)
//working with the le_eixx library (C++ side)

int add(std::vector<int> values)
{ int sum=0; for(auto i: values) sum+=i; return sum; }

//Remember NEVER to use std::cout!!!
//Erlang is using it for communications
void cast1(const char* Msg)
{ std::cerr << "Erlang said " << Msg << std::endl; }

const std::vector<eixx::eterm> cmdline2term(int argc,char *argv[])
{
   std::vector<eixx::eterm> terms;
   for(int i=1;i<argc;i++)
      terms.push_back(le::fmt(argv[i]));

   return terms;
}

int main(int argc, char *argv[])
{
   using namespace eixx;
   using namespace le;
   bool stop;

   //You make a dispatcher object first, which tells the le_eixx library
   //how to handle incoming requests
   //
   //Making a dispatcher is very simple:
   //1. Think of the term patterns you want to use
   //2. Provide a function to handle the matches for each pattern
   //
   //When le receives messages from erlang, it finds the first
   //pattern that matches and calls the function you provided.
   //It takes the term returned by your function and sends it back
   //to erlang.
   //
   //The following example performs the following functions:
   //1.  adds numbers given in different erlang messages: {add,Num} and getsum
   //2.  prints an arbitrary String sent from erlang: {print,Msg}
   //3.  stops the event matching loop when erlang asks with a {stop,Reason} msg
   //4.  Prints a warning message if an unknown msg is received and does not send
   //    anything back to erlang.
   //
   //
   auto dp = le::make_dispatcher(
         "{stop, Reason}",
               [&stop] (varbind& vb) { stop=true;
                                  std::cerr << "Stopping becasue "
                                            << vb["Reason"]->to_string() << std::endl;
                                  return le::nullterm(); }, // returning le:nullterm() allows you to
                                                            // send nothing back to erlang
         le::anyterm(),
               [] (varbind& vb) { std::cerr << "Ignoring strange message";
                                   return le::nullterm(); }
         );

   int i=0;
   while (!stop && i<255)
   {
      for (auto term : cmdline2term(argc,argv)) {
         le::send_term(term);
         //le::dispatch(dp);
         stop=le::check_stop();
      }
      i++;
   }
}
