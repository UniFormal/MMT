# frameit-mmt: Server component of FrameIT project

This is the server component of the [FrameIT project](https://kwarc.info/systems/frameit/), primarily maintained so far by [@ComFreek](https://github.com/ComFreek).

## Installation & Compilation

See [./installation.md](./installation.md).

## REST API

We use JSON for both payload and return values.

### Endpoints

- <details><summary><code>POST /fact/add</code>: make a new fact known to the server</summary>

  - payload: a fact JSON object as detailled above without the "ref" field
  - return value: a fact reference JSON object

  </details>

- <details><summary><code>GET /fact/list</code>: retrieve all facts known to the server</summary>

  - payload: none
  - return value: a JSON array containing fact JSON objects

  </details>

- <details><summary><code>GET /scroll/list</code>: retrieve all scrolls known to the server</summary>

  - payload: none
  - return value: a JSON array containing scroll JSON objects

  </details>

- <details><summary><code>POST /scroll/apply</code>: apply (i.e. use) a scroll and add acquired facts to situation theory</summary>

  - payload: scroll application JSON object
  - return value: a JSON array of fact JSON objects

  </details>

- <details><summary><code>POST /scroll/dynamic</code>: get dynamic information on a scroll given a (possibly partial) scroll application</summary>

  - payload: scroll application JSON object
  - return value: a scroll JSON object
  
  Note that the return value differs from the scroll as output by `/scroll/list` making this endpoint useful after all.
  Namely, all fact and scroll labels, all fact types, and all fact definitions are subject to being dynamically adapted to the (possibly) partial scroll application.
  
  For example, if the original scroll stated `A: point ❘ meta ?MetaAnnotations?label "A" ❙` to be a required fact with label "A"
  and the scroll application maps `A` to `P`  (where `P: point ❘ meta ?MetaAnnotations?label "P"` comes from the situation theory and has label "P"),
  then the dynamic scroll output by this endpoint will state `A: point ❘ meta ?MetaAnnotations?label "P" ❙`.
  The same holds for more complex labels built out of multiple labels of facts. 

  </details>

### Formats

Formats shared by multiple endpoints above.

- <details><summary>Fact reference format</summary>

    ```javascript
    {"uri": /* some uri */}
    ```
    
    Format only given for informational purposes, the game engine should treat fact reference objects opaquely.
    Do not depend on their internal structure.

  </details>

- <details><summary>Fact JSON format</summary>

    - variant a: general facts:
    
      ```javascript
      {
        "ref": /* fact reference */
        "label": "some label",
        "kind": "general",
        "tp": /* OMDoc JSON term */,
        "df": /* OMDoc JSON term or null or left out */
      }
      ```

    - variant b: veq facts
    
      ```javascript
      {
        "ref": /* fact reference */
        "label": "some label",
        "kind": "veq",
        "lhs":   /* OMDoc JSON term */,
        "value": /* OMF OmDoc JSON term */
      }
      ```

  </details>

- <details><summary>Scroll reference format</summary>

    ```javascript
    {
      "problemTheory": /* MMT URI as JSON string */,
      "solutionTheory": /* MMT URI as JSON string */
    }
    ```
  
    Format only given for informational purposes, the game engine should treat scroll reference objects opaquely.
    Do not depend on their internal structure. 

  </details>

- <details><summary>Scroll format</summary>

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

- <details><summary>Scroll application format</summary>

    ```javascript
    {
      "scroll": /* scroll reference */,
      "assignments": [
        ["ref": /* fact reference */, /* OMDoc JSON term (the assignment) */],
        /* ... more elements (same syntax) */
      ]
    }
    ```

  </details>

## Internal REST API

- <details><summary><code>GET /debug/situationtheory/print</code>: output a stringification of the situation theory (and included theories) known to the server</summary>

  - payload: none
  - return value: a JSON string containing MMT surface syntax (probably unparsable by MMT; for human consumption only)

  </details>

## License

Same as the whole MMT source code with the exception of a snippet copied from "the Internet" into `src/info/kwarc/mmt/frameit/communication/ServerErrorHandler.scala`. The precise source and license is given there.
