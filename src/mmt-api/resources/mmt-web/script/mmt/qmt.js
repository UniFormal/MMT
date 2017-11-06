/**
 * An API for QMTResults
 */
var QMTResults = {
    /**
     * Parses a raw result received from the MMT Server
     */
    'fromXML': function(node){
        switch(node.nodeName){
            case 'results':
                return this.parseResults(node);
                break;
            case 'result':
                return this.parseResult(node);
                break;
        }
    },

    /**
     * Unwraps a <results> XML element and turns it into an appropriate array
     * @param node <results> element to parse
     */
    'parseResults': function(node){

        // a list of result nodes
        var resultList = [];

        // iterate over the children of the <result> nodes
        for(var i = 0; i < node.childNodes.length; i++){
            resultList.push(this.parseResult(node.childNodes[i]));
        }

        resultList.html = function(){
            var div = document.createElement('div');

            for(var i = 0; i < node.childNodes.length; i++){
                div.appendChild(resultList[i].html());
            }

            return div;
        };

        // and return the result list
        return resultList;
    },

    /**
     * Unwraps a <result> element and turns it into an appropriate array
     * @param node
     */
    'parseResult': function(node) {

        // a list of elements in the node
        var resultList = [];

        // iterate over the children of the <result> nodes
        for(var i = 0; i < node.childNodes.length; i++){
            resultList.push(this.parseElement(node.childNodes[i]));
        }

        resultList.html = function(){
            // <div class='row'>
            var div = document.createElement('div');
            div.setAttribute('class', 'row');

            // <div class='well'>
            var div2 = document.createElement('div');
            div2.setAttribute('class', 'well');
            div.appendChild(div2);

            for(var i = 0; i < node.childNodes.length; i++){
                div2.appendChild(resultList[i].html());
            }

            return div;
        };

        // and return the result list
        return resultList;
    },

    /**
     * Unwraps a single result (in a list of tuples)
     * @param node Node to unwrap
     */
    'parseElement': function(node) {
        switch(node.nodeName){
            case "uri":
                return new this.URI(node);
                break;
            case "string":
                return new this.String(node);
                break;
            case "object":
                return new this.Object(node);
                break;
            case "xml":
                return new this.XML(node);
                break;
        }
    },



    'URI': function(node){
        this.uri = node.getAttribute("path");

        this.html = function(){
          // <div class='input-group'>
          var div = document.createElement('div');
          div.setAttribute('class', 'input-group');

          // <span class='input-group-addon'>URI</span>
          var span = document.createElement('span');
          span.setAttribute('class', 'input-group-addon');
          span.innerText = 'URI';
          div.appendChild(span);

          // <input type='text' class='form-control' readonly='readonly' value='this.uri'>
          var input = document.createElement('input');
          input.setAttribute('type', 'text');
          input.setAttribute('class', 'form-control');
          input.setAttribute('readonly', 'readonly');
          input.setAttribute('value', this.uri);
          div.appendChild(input);

          return div;
        };
    },

    'String': function(node){
        this.string = node.childNodes[0].textContent;

        this.html = function(){
            // <div class='input-group'>
            var div = document.createElement('div');
            div.setAttribute('class', 'input-group');

            // <span class='input-group-addon'>String</span>
            var span = document.createElement('span');
            span.setAttribute('class', 'input-group-addon');
            span.innerText = 'String';
            div.appendChild(span);

            // <input type='text' class='form-control' readonly='readonly' value='this.string'>
            var input = document.createElement('input');
            input.setAttribute('type', 'text');
            input.setAttribute('class', 'form-control');
            input.setAttribute('readonly', 'readonly');
            input.setAttribute('value', this.string);
            div.appendChild(input);

            return div;
        };
    },

    'Object': function(node){
        this.object = node.innerHTML;

        this.html = function(){
            // <div class='input-group'>
            var div = document.createElement('div');
            div.setAttribute('class', 'input-group');

            // <span class='input-group-addon'>Object</span>
            var span = document.createElement('span');
            span.setAttribute('class', 'input-group-addon');
            span.innerText = 'Object';
            div.appendChild(span);

            // <input type='text' class='form-control' readonly='readonly' value='this.object'>
            var input = document.createElement('input');
            input.setAttribute('type', 'text');
            input.setAttribute('class', 'form-control');
            input.setAttribute('readonly', 'readonly');
            input.setAttribute('value', this.object);
            div.appendChild(input);

            return div;
        };
    },
    'XML': function(node){
        this.xml = node.innerHTML;

        this.html = function(){
            // <div class='input-group'>
            var div = document.createElement('div');
            div.setAttribute('class', 'input-group');

            // <span class='input-group-addon'>XML</span>
            var span = document.createElement('span');
            span.setAttribute('class', 'input-group-addon');
            span.innerText = 'XML';
            div.appendChild(span);

            // <input type='text' class='form-control' readonly='readonly' value='this.xml'>
            var input = document.createElement('input');
            input.setAttribute('type', 'text');
            input.setAttribute('class', 'form-control');
            input.setAttribute('readonly', 'readonly');
            input.setAttribute('value', this.xml);
            div.appendChild(input);

            return div;
        };
    }
};