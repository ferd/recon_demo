# Recon Demo #

## WHAAAT ##

Recon-demo is an app built to demo some of `recon`'s capabilities. It consists
of 26 processes named and registered in gproc (using strings) as:

    Alfa          November
    Bravo         Oscar
    Charlie       Papa
    Delta         Quebec (woo!)
    Echo          Romeo
    Foxtrot       Sierra
    Golf          Tango
    Hotel         Uniform
    India         Victor
    Juliett       Whiskey
    Kilo          Xray
    Lima          Yankee
    Mike          Zulu

Each of them will:

- have a non-deterministic behavior. Each run is different.
- have a TCP connection open and listening
- try to contact each other randomly
- have different levels of chattiness and activity (frequency, msg size)
- will randomly try to send messages over message passing or TCP
- log nothing
- have no particular debugging code in place
- TCP is handled by each process starting its own listen socket, spawning a connector, accepting the connection, having the connector hand over the connected port, and store all of that data in a table (or Gproc) somewhere. Peers can use that connect port to send data to the process, but can't read from it unless they're the owner. This allows to do TCP work where everybody sends using the same connection, and is to be used more as a way to simplify the whole request/response setup. UDP could have been done similarly.

## Questions to Answer ##

- What's the system memory?
- Is the node's CPU loaded?
- Is any process mailbox overflowing?
- Which chatty process takes the most memory?
- Which chatty process is eating the most CPU?
- Which chatty process is consuming the most bandwidth?
- Which chatty process sends the most messages over TCP? The least?
- Can chatty processes message themselves? (can this work with registered names? Do I need to check the chattiest one and see if it messages itself?)
- Can you estimate the overall frequency at which messages are sent globally?


## Building ##

`./rebar get-deps compile`

## Running ##

`./_rel/recon_demo/bin/recon_demo`

