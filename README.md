# <a href="https://docs.trmregistry.com/#/server/README"><img src="https://docs.trmregistry.com/logo.png" height="40" alt="TRM"></a>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-1.3.0-4baaaa.svg)](https://github.com/RegestaItalia/trm-docs/blob/main/CODE_OF_CONDUCT.md)
[![TRM License](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/license/trm-rest)](https://trmregistry.com/package/trm-rest)
[![TRM Latest version](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/version/trm-rest)](https://trmregistry.com/package/trm-rest)
[![TRM Installs](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/downloads/trm-rest)](https://trmregistry.com/package/trm-rest)

| 🚀 This project is funded and maintained by 🏦 | 🔗                                                             |
|-------------------------------------------------|----------------------------------------------------------------|
| Regesta S.p.A.                                  | [https://www.regestaitalia.eu/](https://www.regestaitalia.eu/) |
| Clarex S.r.l.                                   | [https://www.clarex.it/](https://www.clarex.it/)               |

**trm-rest** is the REST wrapper of [trm-server](https://trmregistry.com/package/trm-server).

It acts as a lightweight HTTP layer on top of **trm-server**, exposing the same APIs via REST (instead of RFC).

# Architecture Overview

                  ┌──────────────────────────┐
                  │        TRM Client        │
                  └─────────────┬────────────┘
                                │
                                │  HTTP / REST
                                ▼
                  ┌──────────────────────────┐
                  │         trm-rest         │
                  │  (External REST Layer)   │
                  └─────────────┬────────────┘
                                │
                                │  TRM APIs
                                ▼
                  ┌──────────────────────────┐
                  │        trm-server        │
                  │  (Installed on SAP)      │
                  └─────────────┬────────────┘
                                │
                                │  Native SAP APIs
                                ▼
                  ┌──────────────────────────┐
                  │        SAP System        │
                  └──────────────────────────┘

# Install

First, install [trm-server](https://github.com/RegestaItalia/trm-server) with abapGit, then this repository.

Checkout the install notes [here](https://docs.trmregistry.com/#/server/docs/setup?id=first-install) and repeat the procedure for **trm-rest** when done with **trm-server**.

# Basic Usage

In a typical scenario, TRM Client connects to a SAP system through a **System Connector** (RFC SDK, REST, BTP, etc.).  
After checking that **trm-server** is installed on the system, the client uses its APIs to perform secure and controlled transport export and import operations.  
If not installed, the client can still communicate directly with the SAP system, but with limited capabilities and without extended TRM features.

# Documentation <!-- {docsify-remove} -->

Full documentation can be seen at [https://docs.trmregistry.com](https://docs.trmregistry.com).

To install **trm-server** on your system, [follow the this guide](/docs/setup.md).

# Contributing <!-- {docsify-remove} -->

Like every other TRM open-soruce projects, contributions are always welcomed ❤️.

Make sure to open an issue first.

Contributions will be merged upon approval.

[Click here](https://docs.trmregistry.com/#/CONTRIBUTING) for the full list of TRM contribution guidelines.

[<img src="https://trmregistry.com/public/contributors?image=true">](https://docs.trmregistry.com/#/?id=contributors)
