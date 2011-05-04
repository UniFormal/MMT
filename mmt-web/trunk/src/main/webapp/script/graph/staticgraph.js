var staticgraph = [
      { "id": "C",
        "name": "C",
        "adjacencies": [
            {
              "nodeTo": "FOL",
              "nodeFrom": "C",
              "data": {
                "$type":"arrow",
                "$direction": ["C", "FOL"],
                "uri":"http://cds.omdoc.org/logics/description/translation-fol.omdoc?CFOL"
              }
            }
        ],
        "data": {
          "uri": "http://cds.omdoc.org/logics/description/syntax.omdoc?C",  
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "OrderSup",
        "name": "OrderSup",
        "adjacencies": [
            {
              "nodeTo": "Cocartesian",
              "nodeFrom": "OrderSup",
              "data": {
                "$type":"arrow",
                "$direction": ["OrderSup", "Cocartesian"],
                "uri":"http://cds.omdoc.org/math/orderings/lattice_order.omdoc?Cocartesian?sup"
              }
            }
        ],
        "data": {
          "uri": "http://cds.omdoc.org/math/orderings/order.omdoc?OrderSup",
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "Cocartesian",
        "name": "Cocartesian",
        "adjacencies": [],
        "data": {
          "uri": "http://cds.omdoc.org/math/orderings/lattice_order.omdoc?Cocartesian",
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "Coproduct",
        "name": "Coproduct",
        "adjacencies": [
            {
              "nodeTo": "Product",
              "nodeFrom": "Coproduct",
              "data": {
                "$type":"multiple",
                "param" : 1,
                "$direction": ["Coproduct", "Product"],
                "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?ProdOp"
              }
            },
            {
              "nodeTo": "CoprodTop",
              "nodeFrom": "Coproduct",
              "data": {
                "$type":"arrow",
                "$direction": ["Coproduct", "CoprodTop"],
                "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop?coprod"
              }
            }
        ],
        "data": {
          "uri": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "Product",
        "name": "Product",
        "adjacencies": [
            {
              "nodeTo": "Coproduct",
              "nodeFrom": "Product",
              "data": {
                "$type":"multiple",
                "param" : -1,
                "$direction": ["Product", "Coproduct"],
                "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodOp"
              }
            }
        ],
        "data": {
          "uri": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "CoprodTop",
        "name": "CoprodTop",
        "adjacencies": [],
        "data": {
          "uri": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "SForall",
        "name": "SForall",
        "adjacencies": [
            {
              "nodeTo": "FOL",
              "nodeFrom": "SForall",
              "data": {
                "$type":"arrow",
                "$direction": ["SForall", "FOL"],
                "uri":"http://cds.omdoc.org/translations/sfol-fol/syntax/modules.omdoc?SForallView"
              }
            }
        ],
        "data": {
          "uri": "http://cds.omdoc.org/logics/first-order/syntax/sorted_modules.omdoc?SForall",
          "$type": "circle",
          "$dim": 10
        }
      },
      { "id": "FOL",
        "name": "FOL",
        "adjacencies": [
            {
              "nodeTo": "FOL",
              "nodeFrom": "FOL",
              "data": {
                "$type":"self",
                "param": 1,
                "uri":"http://cds.omdoc.org/translations/sfol-fol/syntax/modules.omdoc?SForallView"
              }
            }
        ],
        "data": {
          "uri": "http://cds.omdoc.org/logics/first-order/syntax/fol.omdoc?FOL",
          "$type": "circle",
          "$dim": 10
        }
      }
  ];