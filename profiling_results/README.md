# Profiling Gingko.

# Profiling Gingko	
1. Start a new instance of Gingko with `./rebar3 shell`. Starting the application from the build console does not include the profiling modules.
2. Set up the data in the application usinng the interfaces.
3. Start the profiler `fprof:start().` This will start the Time profiler.
4. Setup the profiler to trace all the function calls made in the application. `fprof:trace([start,{procs, all}]).`
5. Make the intercae call you want to trace. e.g. `gingko:get_version(%Key,%Type).`
6. After the call is completed, stop the tracing `fprof:trace([stop]).`
7. Then generate the profile data. `fprof:profile().` This might take a while depending on how long the trace is.
8. You can then create the trace output `fprof:analyse({dest, "finename.fprof"}).`
9. This fprof format contains the details of the number of calls along with the percentages of the time spent on individual tasks. Manually reading this could be a hassle. This can be simplified using [QCacheGrind](https://formulae.brew.sh/formula/qcachegrind). 
10. QCacheGrind requires another format which can be generated using [erlgrind](https://github.com/isacssouza/erlgrind). 
11. Convert the fprof generated in step 8 using erlgrind and you can then open it in QCacheGrind.

