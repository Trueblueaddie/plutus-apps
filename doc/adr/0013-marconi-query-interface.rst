.. _marconi_indexer:

ADR 13: Marconi Query Interface
======================================================

Date: 2022-10-27

Author(s)
---------

Kayvan Kazeminejad <kayvan.kazeminejad@iohk.io>

Status
------

Draft

Context
-------
Marconi Query Interface is both an executable and a Haskell library built on top of `marconi`.  In its current alpha version, the interface provides a reporting capability for a bounded list of `Bech32 Shelley Era addresses <https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019#shelley-addresses>`_ and their corresponding `UTXO` `TxOutRef`.  The executable package provides for an HTTP `JSON-RPC 2.0 <https://www.jsonrpc.org/specification>`_ on top of the marconi Haskell libraries.


Scope
-------

The scope of this document is the Marconi query interface executable package built on top:

- `marconi`
- `marconi-query`
- JSON-RPC 2.0
- HTTP

Caution
^^^^^^

This is a living document as the interface is under development.


Decision
--------

We made the following architectural decisions:

* Minimize changes to the marconi back-end
* JSON-RPC 2.0 for non-Haskell users
* Bech32 Shelley Era addresses are provided through CLI
* The alpha release will scope to reporting on `TxOutRef` of `UTXO`s only

These decisions were an architectural compromise with the following and pros cons:

**Pros:**

- Promptly provide the most valued functionality first
- Iterate quickly to provide more functionality
- Built on top of existing Marconi code base with no or minimal impact to Marconi for quick delivery
- Reusability of existing `JSON` encoder/decoder for all entities of interest
- Reasonable real-time query response time for the HTTP JSON-RPC service

**Cons:**

- `SQLite` will remain as Marconi back-end database
- Alpha version **will not** provide:
  - Streaming services
  - Notification services
- Shelley Era addresses only


Implications
^^^^^^

A critical consequence of these compromises in the alpha version is that the query interface must be the sole owner of the SQLite `utxos` database. Any other connection to this SQLite database will result in undefined behavior of the software due to the concurrency nature of SQLite internals.

Reports
------------

From a bird's-eye view the query interface provides a reporting capability for the indexed portion of the blockchain.  Our initial solution with the alpha version provides for reports on the `TxOutRef` of a given address.

Report Request
^^^^^^

A sample JSON-RPC report request using `REST-Client <https://github.com/pashky/restclient.el>`_:

.. code-block:: json

    POST http://localhost:3000/json-rpc
    Content-Type: application/json
    {
    "jsonrpc": "2.0"
    , "method": "txOutRef"
    , "params": "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"
    , "id": 11
    }


Report Request
^^^^^^

.. code-block:: json

    {
      "id": 11,
      "jsonrpc": "2.0",
      "result": [
        {
          "txOutRefId": {
            "getTxId": "083c8678ccebc23d59b315441b37328335c1fc680e2c9f809fac60928fe8cf79"
          },
          "txOutRefIdx": 0
        },
        {
          "txOutRefId": {
            "getTxId": "083c8678ccebc23d59b315441b37328335c1fc680e2c9f809fac60928fe8cf79"
          },
          "txOutRefIdx": 1
        }
      ]
    }
    // POST http://localhost:3000/json-rpc
    // HTTP/1.1 200 OK
    // Transfer-Encoding: chunked
    // Date: Mon, 17 Oct 2022 23:57:50 GMT
    // Server: Warp/3.3.20
    // Content-Type: application/json-rpc
    // Request duration: 0.041375s


Eye Candy
^^^^^^

We have provided a sample JSON-RPC server/client as well as a REST-Client to prototype any cool ideas you might have.  See the `example` folder for more detail.
