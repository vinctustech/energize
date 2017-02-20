---
id: installation
title: Installation
redirect_from: docs/
permalink: docs/installation.html
next: hello-world.html
---

## Installation

### Prerequisites

To run Energize, you'll need Java 1.8 and SBT v0.13.13+. To check that you have the latest JDK, you can run:

```
java -version
```

To check that you have the latest version of SBT, you can run:

```
sbt sbt-version
```

### Clone and run with SBT

The fastest way to get started is to clone the repository and run Energize with SBT.

```
git clone git://github.com/vinctustech/energize.git
cd energize
sbt
```

Once the SBT prompt appears, you can then use the following commands:

- `test` builds and executes tests
- `re-start <config>` starts the server where *config* is the name of the `.energize` file that defines your API. You can press `ENTER` once the server has started to get the SBT prompt in order to enter more SBT commands while the server is running.
- `re-stop` stops the server.