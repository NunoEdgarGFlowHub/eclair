![Eclair Logo](.readme/logo.png)

**Eclair** is a scala implementation of the Lightning Network. Eclair is french for Lightning.

:construction: This branch implements the [Lightning Network Specifications](https://github.com/lightningnetwork/lightning-rfc), it is a work in progress.

[![Build Status](https://travis-ci.org/ACINQ/eclair.svg?branch=wip-bolts)](https://travis-ci.org/ACINQ/eclair)

---

## Overview

This software creates a node compatible with a Lightning network and provides a GUI to manage the node. A JSON-RPC API is available if you prefer to run it headless.

Available actions:
- Open a channel with another eclair or lightningd instance
- Display opened channels with the node (status, balance, capacity, ...)
- Receive payments from another node
- Send payments to another node

![Eclair Demo](.readme/screen-1.png)

## Installation

The project is under heavy development and no release is available yet. Still you can download the sources, compile the project with Maven (cf §Development) and run it on localhost.

---

## Development

#### Set up the environment
- JDK 1.8+
- [Latest Scala installation](http://www.scala-lang.org/download/)
- [Maven](https://maven.apache.org/download.cgi)
- A segwit version of bitcoin core in testnet or regtest mode

:warning: eclair currently runs on regtest/segnet only. **Do not try and modify it to run on bitcoin mainnet!**

- Make sure that bitcoin-cli is on the path and edit ~/.bitcoin/bitcoin.conf and add:
```
server=1
regtest=1
rpcuser=***
rpcpassword=***
```

#### Run

- Download the sources and build the executable JAR with the following command:
```shell
$ mvn package -DskipTests
```
- Start bitcoind
- Mine enough blocks to activate segwit blocks:
```shell
$ bitcoin-cli generate 500
```
- Navigate to `eclair-node/target` and execute the jar `eclair-node_2.11-0.2-SNAPSHOT-xxxxxx-capsule-fat.jar`
````shell
$ java 
     -Declair.bitcoind.rpcuser=foo
     -Declair.bitcoind.rpcpassword=bar
     -jar eclair-node_2.11-0.2-SNAPSHOT-xxxxxx-capsule-fat.jar
```

#### JVM Options

option                       | default value             | description
-----------------------------|---------------------------|---------
 eclair.server.port          | TCP port                  | 9735
 eclair.http.port            | HTTP port                 | 8080
 eclair.bitcoind.rpcuser     | Bitcoin Core RPC user     | foo
 eclair.bitcoind.rpcpassword | Bitcoin Core RPC password | bar


&rarr; see [`application.conf`](eclair-node/src/main/resources/application.conf) for full reference.

#### Testing with lightningd

&rarr; Checkout [our guide](TESTING.md)

## JSON-RPC API

 method       |  params                             | description
 -------------|-------------------------------------|-----------------------------------------------------------
  connect     | host, port, anchor_amount           | opens a channel with another eclair or lightningd instance
  list        |                                     | lists existing channels
  addhtlc     | channel_id, amount, rhash, locktime | sends an htlc
  fulfillhtlc | channel_id, r                       | fulfills an htlc
  close       | channel_id                          | closes a channel
  help        |                                     | displays available methods

---

## Project Status
- [X] Network
- [X] Routing (simple IRC prototype)
- [X] Channel protocol
- [X] HTLC Scripts
- [X] Unilateral close handling
- [X] Relaying Payment
- [ ] Fee management
- [X] Blockchain watcher
- [ ] Storing states in a database

## Resources
- [1]  [The Bitcoin Lightning Network: Scalable Off-Chain Instant Payments](https://lightning.network/lightning-network-paper.pdf) by Joseph Poon and Thaddeus Dryja
- [2]  [Reaching The Ground With Lightning](https://github.com/ElementsProject/lightning/raw/master/doc/deployable-lightning.pdf) by Rusty Russell

## Other implementations
Name         | Language | Compatible
-------------|----------|------------
[Amiko-Pay]  | Python   | no
[lightning-c]| C        | yes
[lnd]        | Go       | no
[Thunder]    | Java     | no

[Amiko-Pay]: https://github.com/cornwarecjp/amiko-pay
[lightning-c]: https://github.com/ElementsProject/lightning
[lnd]: https://github.com/LightningNetwork/lnd
[Thunder]: https://github.com/blockchain/thunder
