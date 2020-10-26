# frameit-mmt: Server component of FrameIT project

This is the server component of the [FrameIT project](https://kwarc.info/systems/frameit/), primarily maintained so far by [@ComFreek](https://github.com/ComFreek).

## Installation & Compilation

See [./installation.md](./installation.md).

## REST API

We use UTF-8-encoded JSON payloads for both HTTP request and response bodies.

All endpoints indicate success by a 2xx response status code and failure by any other status code.

### Endpoints

- <details><summary><code>POST /fact/add</code>: make a new fact known to the server</summary>

  - request: a fact JSON object (without the "ref" field)
  - response: a fact reference

  </details>

- <details><summary><code>GET /fact/list</code>: retrieve all facts known to the server</summary>

  - request: empty
  - response: a JSON array of facts

  </details>

- <details><summary><code>GET /scroll/list</code>: retrieve all scrolls known to the server</summary>

  - request: empty
  - response: a JSON array of scrolls

  </details>

- <details><summary><code>POST /scroll/apply</code>: apply (i.e. use) a scroll and add acquired facts to situation theory</summary>

  - request: a scroll application
  - response: a JSON array of facts

  </details>

- <details><summary><code>POST /scroll/dynamic</code>: get dynamic information on a scroll given a (possibly partial) scroll application</summary>

  - request: a scroll application
  - response: a scroll
  
  Note that the return value differs from the scroll as output by `/scroll/list` making this endpoint useful after all.
  Namely, all fact and scroll labels, all fact types, and all fact definitions are subject to being dynamically adapted to the (possibly) partial scroll application.
  
  For example, if the original scroll stated `A: point ❘ meta ?MetaAnnotations?label "A" ❙` to be a required fact with label "A"
  and the scroll application maps `A` to `P`  (where `P: point ❘ meta ?MetaAnnotations?label "P"` comes from the situation theory and has label "P"),
  then the dynamic scroll output by this endpoint will state `A: point ❘ meta ?MetaAnnotations?label "P" ❙`.
  The same holds for more complex labels built out of multiple labels of facts. 

  </details>

### Formats

JSON (sub)formats shared by multiple endpoints above.

- <details><summary>fact reference</summary>

    ```javascript
    {"uri": /* MMT URI */}
    ```
    
    Format only given for informational purposes, the game engine should treat fact reference objects opaquely.
    Do not depend on their internal structure.

  </details>

- <details><summary>fact</summary>

    - variant a: general facts:
    
      ```javascript
      {
        "ref": /* fact reference */
        "label": "some label",
        "kind": "general",
        "tp": /* SOMDoc */,
        "df": /* SOMDoc or null or left out */
      }
      ```

    - variant b: veq facts
    
      ```javascript
      {
        "ref": /* fact reference */
        "label": "some label",
        "kind": "veq",
        "lhs":   /* SOMDoc */,
        "value": /* SOMDoc (must be an OMF) */
      }
      ```

  </details>

- <details><summary>scroll reference</summary>

    ```javascript
    {
      "problemTheory": /* MMT URI */,
      "solutionTheory": /* MMT URI */
    }
    ```
  
    Format only given for informational purposes, the game engine should treat scroll reference objects opaquely.
    Do not depend on their internal structure. 

  </details>

- <details><summary>scroll</summary>

    ```javascript
    {
      "ref": /* scroll reference */,
      "label": "some label",
      "description": "some description",
      "requiredFacts": /* array of facts; facts that the scroll required you to give for scroll application */
      "acquiredFacts": /* array of facts; facts that the scroll gives you upon successful scroll application */
    }
    ```

  </details>

- <details><summary>scroll application</summary>

    ```javascript
    {
      "scroll": /* scroll reference */,
      "assignments": [
        ["ref": /* fact reference */, /* SOMDoc (the assigned term to the fact slot) */],
        /* ... more elements (same syntax) */
      ]
    }
    ```

  </details>

- <details><summary>SOMDoc ("simplified OMDoc")</summary>

    We follow the (insert link here to omdoc json standard) with one addition elaborated on below.
    For quick reference, here is a representative sample of SOMDoc:
    
    - `{"kind": "OMS", "uri": "..."}`
    - `{"kind": "OMA", "applicant": /* SOMDoc */, "arguments": /* array of SOMDoc */}`
    - `{"kind": "OMI", "value": 42}`
    - `{"kind": "OMF", "float": 0.1234}`
    - `{"kind": "OMSTR", "string": "string in UTF-8"}`
    - `{"kind": "RAW", "xml": "OMDoc XML as string in UTF-8"}` (our addition to the (insert link here to omdoc json standard))

  </details>

- <details><summary>MMT URI</summary>

    A JSON string representing an MMT URI. We follow the string representation of MMT URIs as implemented in MMT itself.

  </details>

## Internal REST API

- <details><summary><code>GET /debug/situationtheory/print</code>: output a stringification of the situation theory (and included theories) known to the server</summary>

  - request: empty
  - response a JSON string containing MMT surface syntax (probably unparsable by MMT; for human consumption only)

  </details>

## License

Same as the whole MMT source code with the exception of a snippet copied from "the Internet" into `src/info/kwarc/mmt/frameit/communication/ServerErrorHandler.scala`. The precise source and license is given there.
