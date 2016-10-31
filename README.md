# Temperature control with Raspberry Pi & Akka 
This is the source code for my hobby project controlling radiator valves using the Raspberry Pi. I gave a talk about this at [Scala.io](http://scala.io) and you can find the slides [here](https://docs.google.com/presentation/d/1nNsFkdE_LWkUUwBnu7v4Zz_7J2EaoKhJWRfDw1fCSZU/pub?start=false&loop=false&delayms=3000). 

## Current state
- Successfully receiving and decoding data.
- Successful teach in.
- Successful responding setting new valve position.

You can find the current code on the [enocean-flow-sample](https://github.com/tbje/enocean-pi-akka/tree/enocean-flow-sample) branch.

## TODO
- Extract code from Flow terminal app and move to master branch.
- Client for setting desired valve pos.
- Extractors and case classes for extracting data.
- Delegate responsibilities to different actors.
- Add REPL code into tests.












