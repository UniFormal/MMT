var staticgraph = [
  { 
    "id": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
    "name": "Coproduct",
    "adjacencies": [
      {
        "nodeTo": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
        "nodeFrom": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
        "data": {
          "$type":"multiple_arrow",
          "$direction": [
            {
              "from": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
              "to": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
              "kind": "View", 
              "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodOp"  
            },
            {
              "from": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
              "to": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
              "kind": "View", 
              "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?ProdOp"  
            },            
          ]                
        }
      },
      {
        "nodeTo": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
        "nodeFrom": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
        "data": {
          "$type":"labeled_arrow",
          "kind": "Structure",
          "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop?coprod"
        }
      }
    ],
    "data": {
      "kind": "Theory",  
      "uri": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Coproduct",
      "$type": "circle",
      "$dim": 10
    }
  },
  {
    "id": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
    "name": "CoprodTop",
    "adjacencies": [
      {
        "nodeTo": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
        "nodeFrom": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
        "data": {
          "$type":"double_arrow",
          "kind": "Structure",
          "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop?prod"
        }
      },
      {
        "nodeFrom": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
        "nodeTo": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
        "data": {
          "$type":"self_arrow",
          "kind": "View",
          "uri":"http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodT"
        }
      }             
    ],
    "data": {
      "kind": "Theory", 
      "uri": "http://cds.omdoc.org/category_theory/lf-based.omdoc?CoprodTop",
      "$type": "circle",
      "$dim": 10
    }
  },
  {
    "id": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
    "name": "Product",
    "adjacencies": [],
    "data": {
      "kind": "Theory", 
      "uri": "http://cds.omdoc.org/category_theory/lf-based.omdoc?Product",
      "$type": "circle",
      "$dim": 10
    }
  }
];
