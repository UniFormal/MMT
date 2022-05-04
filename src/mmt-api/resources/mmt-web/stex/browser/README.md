# TreeJS

TreeJS is a simple JavaScript library, to display a TreeView like in the windows filebrowser.
It implements partially the Java Swing TreeModel etc.

**Demo:** https://prod.thalmann.it/treejs/demo.html

## Navigation
- [Installation](#installation)
- [Usage](#usage)
- [Documentation](#documentation)
  - [TreeView](#treeview)
  - [TreeNode](#treenode)
  - [TreePath](#treepath)
  - [TreeUtil](#treeutil)
  - [TreeConfig](#treeconfig)
  - [Events](#events)
  - [Options](#options)
- [Example](#example)

## Installation
1. Download the .zip-File and put it in your project-folder.

2. Add this script-tag to the head of the file
```html
<script src="path/to/js/file.js"></script>
```

3. Add this link-tag to the head of the file, to include the styles
```html
<link rel="stylesheet" href="path/to/css/file.css" />
```

4. Start using the library!

## Usage
### Create new TreeView
```javascript
var root = new TreeNode("root"); // Create the root-node
var tree = new TreeView(root);   // Create the tree
```

### Set a container to display the tree
```javascript
tree.setContainer("#container"); // Uses document.querySelector
```
or
```javascript
tree.setContainer(document.getElementById("container"));
```

### (Re-)load the tree
```javascript
tree.reload(); // Always use this, when you change the TreeView or any of its nodes
```
## Documentation
### TreeView
It's the main object to display the Tree.
#### Instanciating
```javascript
new TreeView(root, container, options);
```
- **root** (TreeNode): The root-node of the tree.
- **container** (DOM-Element/querySelector): The container for the tree to display **(optional)**
- **options** (object): A object with options for the tree (see [below](#options)) **(optional)**

After the instanciation, the TreeView is reloaded/rendered

#### Methods
```javascript
tree.setRoot(root);                  // Resets the root-node (TreeNode)
tree.getRoot();                      // Returns the root-node

tree.expandAllNodes();               // Expands all nodes of the tree
tree.expandPath(path);               // Expands all nodes that are in the path (TreePath)
tree.collapseAllNodes();             // Collapses all nodes of the tree

tree.setContainer(container);        // Resets the container (DOM-Element/querySelector)
tree.getContainer();                 // Returns the container

tree.setOptions(options);            // Resets the options (object)
tree.changeOption(option, value);    // Changes one option (string, object)
tree.getOptions();                   // Returns the options

tree.getSelectedNodes();             // Returns all selected nodes in the tree
tree.reload();                       // Reloads/Renders the tree inside of the container
```

### TreeNode
It represents a node inside of a tree. You can append children to it and specify a userobject, which is used to display text on a node. This object can be a string but can also be a other object, where the toString() function is used to display the text.
#### Instanciating
```javascript
new TreeNode(userobject, options);
```

- **userobject** (object): This object is used to display text on the node (if not string, toString() is used) **(optional)**
- **options** (object): A object with options for the node (see [below](#options)) **(optional)**

#### Methods
```javascript
node.addChild(node);                 // Adds a child to the current node and sets the parent of the node (TreeNode)
node.removeChildPos(pos);            // Removes the child at this position (integer)
node.removeChild(node);              // Removes the child from the current node, if contained (TreeNode)
node.getChildren();                  // Returns a array with the children of the current node
node.getChildCount();                // Returns the number of children
node.getIndexOfChild(node);          // Returns the position of the child; -1 is returned if not found (TreeNode)

node.getRoot();                      // Tries to get the root node of this node

node.setUserObject(userobject);      // Resets the userobject (object)
node.getUserObject();                // Returns the userobject

node.setOptions(options);            // Resets the options (object)
node.changeOption(option, value);    // Changes one option (string, object)
node.getOptions();                   // Returns the options

node.isLeaf();                       // Returns true, if the node doesn't have any children, else false

node.setExpanded(true|false);        // Sets the expanded-state of the node (if it shows its children) (boolean)
node.toggleExpanded();               // Toggles the expanded-state of the node
node.isExpanded();                   // Returns, if the node is expanded or not

node.setEnabled(true|false);         // Sets the enabled-state of the node (if it is enabled) (boolean)
node.toggleEnabled();                // Toggles the enabled-state of the node
node.isEnabled();                    // Returns, if the node is enabled or not

node.setSelected(true|false);        // Sets the selected-state of the node (if it is selected) (boolean)
node.toggleSelected();               // Toggles the selected-state of the node
node.isSelected();                   // Returns, if the node is selected or not

node.open();                         // Triggers the "open"-event of the node

node.on(event, callback);            // Sets the eventlistener of the event, if the callback is specified;
                                     // if only the event is set, it returns the callback-function; if that is not
                                     // set, it returns a empty function (string, function)
node.getListener(event);             // Returns the callback-function for this event, if set (string)

node.equals(node);                   // Returns if the node is equal to the parameter (TreeNode)

node.toString();                     // Returns the generated string from the userobject
```

### TreePath
It represents a path inside of a tree (containing all nodes that form the path).
#### Instanciating
```javascript
new TreePath(root, node);
```

- **root** & **node** (TreeNode): if they are both set, the method setPath(root, node) is executed **(optional)**

#### Methods
```javascript
path.setPath(root, node);            // Generates the path between root and node (TreeNode, TreeNode)
path.getPath();                      // Returns the generated path as a array

path.toString();                     // Returns the path as a string (nodes joined with a ' - ')
```

### TreeUtil
A collection of default values and methods. Can't be instanciated.
#### Variables
```javascript
TreeUtil.default_leaf_icon           // String, that represents the default icon for a leaf-node
TreeUtil.default_parent_icon         // String, that represents the default icon for a normal node (with children)
TreeUtil.default_open_icon           // String, that represents the default expanded-icon
TreeUtil.default_close_icon          // String, that represents the default collapsed-icon
```
#### Methods
```javascript
TreeUtil.isDOM(object);              // Returns true, if the object is a DOM-Element, else false (object)

TreeUtil.getProperty(opt, o, def);   // Returns the value of 'o' in the array/object opt, if it is set;
                                     // else it returns def (object, string, object)
TreeUtil.expandNode(node);           // Expands the node and all it's children and theirs etc. (TreeNode)
TreeUtil.collapseNode(node);         // Collapses the node and all it's children and theirs etc. (TreeNode)

TreeUtil.getSelectedNodesForNode(n); // Returns all selected nodes inside of this node (and it's self,
                                     // if its selected) (TreeNode)
```

### TreeConfig
A collection of values, that you can change (directly inside of the file or with JavaScript for only one site).
#### Variables
```javascript
TreeConfig.leaf_icon                 // The icon for a leaf-node (default: TreeUtil.default_leaf_icon)
TreeConfig.parent_icon               // The icon for a normal node (default: TreeUtil.default_parent_icon)
TreeConfig.open_icon                 // The expanded-icon (default: TreeUtil.default_open_icon)
TreeConfig.close_icon                // The collapsed-icon (default: TreeUtil.default_close_icon)
TreeConfig.context_menu              // A function that is executed when a contextmenu is opened (default: undefined)
```

### Events
It is possible to attach a event to a TreeNode: ``node.on(event, callback);``

| Event | Callback-Parameter(s) | Definition | Restriction |
|-----------------|--------------------------------------|--------------------------------------------------------------------------|---------------|
| click | e[click_event], node[TreeNode] | Is triggered when the node is clicked | - |
| expand | node[TreeNode] | Is triggered when the node is expanded | Not-leaf only |
| collapse | node[TreeNode] | Is triggered when the node is collapsed | Not-leaf only |
| toggle_expanded | node[TreeNode] | Is triggered when the node is either expanded or collapsed | Not-leaf only |
| open | node[TreeNode] | Is triggered when the open()-Function is executed or the leaf is clicked | Leaf only |
| enable | node[TreeNode] | Is triggered when the node is enabled | - |
| disable | node[TreeNode] | Is triggered when the node is disabled | - |
| toggle_enabled | node[TreeNode] | Is triggered when the node is either enabled or disabled | - |
| select | node[TreeNode] | Is triggered when the node is selected | - |
| deselect | node[TreeNode] | Is triggered when the node is deselected | - |
| toggle_selected | node[TreeNode] | Is triggered when the node is either selected or deselected | - |
| contextmenu | e[contextmenu_event], node[TreeNode] | Is triggered when a contextmenu is opened on a node | - |

### Options
#### for TreeView

| Option | Values | Definition |
|-------------|----------|-----------------------------------------------------------------------------|
| leaf_icon | [string] | Sets the leaf-icon for this tree to the string (can be overwritten by node) |
| parent_icon | [string] | Sets the node-icon for this tree to the string (can be overwritten by node) |
| show_root | [boolean] | Sets whether the root node is shown or not |

#### for TreeNode

| Option | Values | Definition |
|----------------|------------|----------------------------------------------------------------|
| expanded | [boolean] | On creation, the node will have the expanded value set to this |
| enabled | [boolean] | On creation, the node will have the enabled value set to this |
| selected | [boolean] | On creation, the node will have the selected value set to this |
| icon | [string] | Sets the icon for this node to the string |
| allowsChildren | [boolean] | Sets if there can be added new children to this node |
| forceParent | [boolean] | This node will be displayed as parent, even if it is empty |

## Example
### Code:
```javascript
var root = new TreeNode("root");
			var n1 = new TreeNode("1");
				var n11 = new TreeNode("1.1");
			var n2 = new TreeNode("2");
			var n3 = new TreeNode("3");
				var n31 = new TreeNode("3.1");
				var n32 = new TreeNode("3.2");
					var n321 = new TreeNode("3.2.1");
				var n33 = new TreeNode("3.3");
        
root.addChild(n1);
root.addChild(n2);
root.addChild(n3);

n1.addChild(n11);

n3.addChild(n31);
n3.addChild(n32);
n3.addChild(n33);

n32.addChild(n321);

n3.setEnabled(false);

var view = new TreeView(root, "#container");
```

### Output:

![treeJs example](demo/example.jpg)
