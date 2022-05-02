# frameit-mmt: Server component of FrameIT project

This is the server component of the [FrameIT project](https://uframeit.org), primarily maintained by [@ComFreek](https://github.com/ComFreek).

## Installation

- End users: you most likely want to use a [pre-packaged UFrameIT release](https://github.com/UFrameIT/UFrameIT/releases)
- Developers: follow [./DEV.md](DEV.md).

## REST API

We use UTF-8-encoded JSON payloads for both HTTP request and response bodies.
All endpoints indicate success by a 2xx response status code and failure by any other status code.

**Playground.**
The [kwarc research group](https://kwarc.info) uses [Postman](https://www.postman.com/) to collaboratively tinker with the request & response API.
We maintain a [Postmean team](https://kwarc-frameit.postman.co/home) and a [public Postman workspace](https://www.postman.com/kwarc-frameit/workspace/frameit-developers-workspace/overview) with sample request & response collections of the REST API.
To request write access, contact one of the [UFrameIT team members](https://uframeit.org/).
Currently, [@ComFreek](https://github.com/ComFreek) and [@SESch93](https://github.com/SESch93) have and manage write access.
(Unfortunately, in the free version of Postman, team size is limited to two people.)
As a backup, in [`./mmt-frameit-server-2021-02-17.postman_collection.json`](./mmt-frameit-server-2021-02-17.postman_collection.json) you can find a snapshot of an exported version of the workspace's contents.

### Endpoints

- <details><summary><code>POST /fact/add</code>: make a new fact known to the server</summary>

  - request: a fact JSON object (without the "ref" field)
  - response: a fact reference

  </details>

- <details><summary><code>GET /fact/list</code>: retrieve all facts known to the server</summary>

  - request: empty
  - response: a JSON array of facts

  </details>

- <details><summary><code>GET /scroll/list</code>: retrieve all scrolls accessible from the current situation theory</summary>

  - request: empty
  - response: a JSON array of scrolls

  </details>

- <details><summary><code>GET /scroll/listall</code>: retrieve all scrolls known to the server</summary>

  - request: empty
  - response: a JSON array of scrolls

  </details>

- <details><summary><code>POST /scroll/apply</code>: apply (i.e. use) a scroll and add acquired facts to situation theory</summary>

  - request: a scroll application
  - <details><summary>response: scroll application info</summary>
  
    ```javascript
    {
        "valid": true|false,
        "errors": /* an array of "scroll application checking error"s */,
        "acquiredFacts": /* an array of facts */
    }
    ```
    
    Invariant: valid = false => errors.nonEmpty
    
    </details>
    
  </details>

- <details><summary><code>POST /scroll/dynamic</code>: get dynamic information on a scroll given a (possibly partial) scroll application</summary>

  - request: a scroll application
  - <details><summary>response: dynamic scroll info</summary>
  
    ```javascript
    {
        "original": /* a scroll */,
        "rendered": /* a scroll */,
        "completions": /* an array of "scroll assignments lists" */,
        "valid": true|false,
        "errors": /* an array of "scroll application checking error"s */
    }
    ```
    
    Invariant: valid = false => errors.nonEmpty
  
    The scroll under *original* contains the original scroll.
    By contrast, in *rendered* all fact and scroll labels, all fact types, and all fact definitions are subject to being dynamically adapted to the (possibly utterly partial) scroll application.
  
    For example, if the original scroll stated `A: point ❘ meta ?MetaAnnotations?label "A" ❙` to be a required fact with label "A"
  and the scroll application maps `A` to `P`  (where `P: point ❘ meta ?MetaAnnotations?label "P"` comes from the situation theory and has label "P"),
  then the dynamic scroll output by this endpoint will state `A: point ❘ meta ?MetaAnnotations?label "P" ❙`.
    The same holds for more complex labels built out of multiple labels of facts.
  
     Furthermore, *completions* is an array of scroll view completion suggestions for the yet missing assignments.
    For instance, the first element of *completions* might be a list of some (possibly not all) of the yet missing assignments of the
    request's scroll view. Analogously for the other elements of *completions*, if they exist.
  It might happen that multiple mutually exclusive *completions* exist, hence the response contains an array of them.

    </details>
    
  </details>

### Formats

JSON (sub)formats shared by multiple endpoints above.

- <details><summary>fact reference</summary>

    ```javascript
    {"uri": /* MMT URI */}
    ```
    
    The game engine may depend on this format (in contrast to, e.g., the format of scroll references).

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
    /* MMT URI to theory declaring the scroll */
    ```
  
    Format only given for informational purposes, the game engine should treat JSON blobs of scroll references opaquely.
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


- <details><summary>scroll assignments list</summary>

    ```javascript
    [
      {
         "fact": /* a fact reference */,
         "assignment": /* SOMDoc */
      },
      /* more entries like the above
    ]
    ```

  </details>

- <details><summary>scroll application</summary>

    ```javascript
    {
      "scroll": /* scroll reference */,
      "assignments": /* a scroll assignments list */
    }
    ```

  </details>

- <details><summary>scroll application checking error</summary>

    ```javascript
    {
      "kind": "invalidAssignment" | "unknown",
      "msg": /* some human-readable message */,
  
      /* in case of kind being "invalidAssignment": */
      "fact": /* a fact reference to the fact whose assignment was erroneous */
    }
    ```

  </details>

- <details><summary>SOMDoc ("simplified OMDoc")</summary>

    SOMDoc is a JSON representation of a subset of [OMDoc](https://www.omdoc.org/). It is simpler than the [OpenMath-JSON standard](https://omjson.kwarc.info/) and *almost* implements a subset of it.
    Below is a representative list of all possible SOMDoc terms as JSON:
    
    - `{"kind": "OMS", "uri": /* MMT  URI */}`
    - `{"kind": "OMA", "applicant": /* SOMDoc */, "arguments": /* array of SOMDoc */}`
    - `{"kind": "OMI", "decimal": 42}`
    - `{"kind": "OMF", "float": 0.1234}`
    - `{"kind": "OMSTR", "string": "string in UTF-8"}`
    - `{"kind": "RAW", "xml": "OMDoc XML as string in UTF-8"}` (our addition to the (insert link here to omdoc json standard))
    
    In contrast to OpenMath-JSON, OMS terms simply encode the full MMT URI as a string instead of specifying its components separately. (E.g., OpenMath-JSON would provide fields `cd`, `cdbase`, and `name`.)
    Moreover, as all but the last bullet point above only represent a subset of OMDoc, we need a way to encode unrepresented terms: we do so by `{kind: "RAW", "xml": "..."}`.

  </details>

- <details><summary>MMT URI</summary>

    A JSON string representing an MMT URI. We follow the string representation of MMT URIs as implemented in MMT itself.

  </details>

## Internal REST API

- <details><summary><code>POST /fact/bulkadd</code>: adds multiple facts at once</summary>

  - request: a JSON array of fact JSON objects (each without the "ref" field)

  - response: a JSON array with elements of the form of either `[/* fact reference */, ""]` or `["error string..."]`

  </details>

- <details><summary><code>GET /debug/space/print</code>: output a stringification of the situation theory (and included theories) known to the server</summary>

  - request: empty
  - response: an HTTP response with content type `text/plain` (not JSON!) and with body a dump in MMT surface syntax of the situation theory. The dump is probably unparsable by MMT; meant for human consumption only.

  </details>

- <details><summary><code>GET /debug/space/check</code>: typecheck situation space</summary>

  - request: empty
  - response: a Json response with list of errors

  </details>

## Development

Currently, the primary maintainer and author of most of the code is [@ComFreek](https://github.com/ComFreek). See <./DEV.md> for particularly important developer's notes.

## License

Same as the whole MMT source code with the exception of a snippet copied from "the Internet" into `src/info/kwarc/mmt/frameit/communication/ServerErrorHandler.scala`. The precise source and license is given there.
