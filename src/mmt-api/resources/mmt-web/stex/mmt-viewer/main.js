"use strict";
(self["webpackChunk"] = self["webpackChunk"] || []).push([["main"],{

/***/ 13761:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "App": () => (/* binding */ App),
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _stex_react_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(61202);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/apps/mmt-viewer/src/app/app.tsx";


function App() {
  const contentUrl = window.BASE_URL + window.CONTENT_URL;
  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_1__.jsxDEV)(_stex_react_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__.StexReactRenderer, {
    contentUrl: contentUrl
  }, void 0, false, {
    fileName: _jsxFileName,
    lineNumber: 5,
    columnNumber: 10
  }, this);
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (App);

/***/ }),

/***/ 61871:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(2784);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var react_dom_client__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(17029);
/* harmony import */ var _app_app__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(13761);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/apps/mmt-viewer/src/main.tsx";




const root = react_dom_client__WEBPACK_IMPORTED_MODULE_1__.createRoot(document.getElementById('root'));
root.render( /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(react__WEBPACK_IMPORTED_MODULE_0__.StrictMode, {
  children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(_app_app__WEBPACK_IMPORTED_MODULE_2__["default"], {}, void 0, false, {
    fileName: _jsxFileName,
    lineNumber: 11,
    columnNumber: 5
  }, undefined)
}, void 0, false, {
  fileName: _jsxFileName,
  lineNumber: 10,
  columnNumber: 3
}, undefined));

/***/ }),

/***/ 61202:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "BG_COLOR": () => (/* reexport safe */ _lib_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__.BG_COLOR),
/* harmony export */   "ContentFromUrl": () => (/* reexport safe */ _lib_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__.ContentFromUrl),
/* harmony export */   "StexReactRenderer": () => (/* reexport safe */ _lib_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__.StexReactRenderer),
/* harmony export */   "mmtHTMLToReact": () => (/* reexport safe */ _lib_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__.mmtHTMLToReact)
/* harmony export */ });
/* harmony import */ var _lib_stex_react_renderer__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(36911);


/***/ }),

/***/ 6404:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "ContentFromUrl": () => (/* binding */ ContentFromUrl)
/* harmony export */ });
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(15735);
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(6886);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(30750);
/* harmony import */ var axios__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(89644);
/* harmony import */ var axios__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(axios__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var _mmtParser__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(95943);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(2784);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_4___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_4__);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/libs/stex-react-renderer/src/lib/ContentFromUrl.tsx";








function ContentFromUrl({
  url,
  modifyRendered = n => n,
  skipSidebar = false
}) {
  const [rendered, setRendered] = (0,react__WEBPACK_IMPORTED_MODULE_4__.useState)( /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.Fragment, {}, void 0, false));
  const [isLoading, setIsLoading] = (0,react__WEBPACK_IMPORTED_MODULE_4__.useState)(false);
  const [highlightedParentId, setHighlightedParentId] = (0,react__WEBPACK_IMPORTED_MODULE_4__.useState)("");
  const value = (0,react__WEBPACK_IMPORTED_MODULE_4__.useMemo)(() => ({
    highlightedParentId,
    setHighlightedParentId
  }), [highlightedParentId]);
  (0,react__WEBPACK_IMPORTED_MODULE_4__.useEffect)(() => {
    setIsLoading(true);
    axios__WEBPACK_IMPORTED_MODULE_2___default().get(url).catch(_e => null).then(r => {
      setIsLoading(false);
      let toShow;
      if (r) toShow = (0,_mmtParser__WEBPACK_IMPORTED_MODULE_3__.mmtHTMLToReact)(r.data, skipSidebar);else toShow = /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.jsxDEV)("span", {
        style: {
          color: "red"
        },
        children: ["Error loading: ", url]
      }, void 0, true, {
        fileName: _jsxFileName,
        lineNumber: 33,
        columnNumber: 23
      }, this);
      setRendered(toShow);
    });
  }, [url, skipSidebar]);

  if (isLoading) {
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.Fragment, {
      children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.jsxDEV)("span", {
        style: {
          fontSize: "smaller"
        },
        children: url
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 41,
        columnNumber: 9
      }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_6__["default"], {}, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 42,
        columnNumber: 9
      }, this)]
    }, void 0, true);
  }

  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_5__.jsxDEV)(_mmtParser__WEBPACK_IMPORTED_MODULE_3__.HighlightContext.Provider, {
    value: value,
    children: modifyRendered(rendered)
  }, void 0, false, {
    fileName: _jsxFileName,
    lineNumber: 47,
    columnNumber: 5
  }, this);
}

/***/ }),

/***/ 74061:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "ExpandableContent": () => (/* binding */ ExpandableContent)
/* harmony export */ });
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(15735);
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(6886);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var _mui_icons_material_AddBoxOutlined__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(71963);
/* harmony import */ var _mui_icons_material_IndeterminateCheckBoxOutlined__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(59169);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(42921);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(87037);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(2784);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var _ContentFromUrl__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(6404);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/libs/stex-react-renderer/src/lib/ExpandableContent.tsx";








function ExpandableContent({
  contentUrl,
  title
}) {
  const [openAtLeastOnce, setOpenAtLeastOnce] = (0,react__WEBPACK_IMPORTED_MODULE_2__.useState)(false);
  const [isOpen, setIsOpen] = (0,react__WEBPACK_IMPORTED_MODULE_2__.useState)(false);

  const changeState = () => {
    setOpenAtLeastOnce(true);
    setIsOpen(v => !v);
  };

  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_5__["default"], {
    m: "4px 0",
    children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_5__["default"], {
      display: "flex",
      alignItems: "center",
      sx: {
        cursor: 'pointer'
      },
      onClick: changeState,
      children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_6__["default"], {
        sx: {
          color: 'gray',
          p: '0'
        },
        onClick: e => {
          e.stopPropagation();
          changeState();
        },
        children: isOpen ? /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_icons_material_IndeterminateCheckBoxOutlined__WEBPACK_IMPORTED_MODULE_7__["default"], {
          sx: {
            fontSize: '20px'
          }
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 38,
          columnNumber: 13
        }, this) : /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_icons_material_AddBoxOutlined__WEBPACK_IMPORTED_MODULE_8__["default"], {
          sx: {
            fontSize: '20px'
          }
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 40,
          columnNumber: 13
        }, this)
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 30,
        columnNumber: 9
      }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_5__["default"], {
        sx: {
          '&:hover': {
            background: '#DDD'
          },
          width: 'fit-content',
          px: '4px',
          ml: '-2px',
          borderRadius: '5px'
        },
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)("b", {
          style: {
            fontSize: 'large'
          },
          children: title
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 52,
          columnNumber: 11
        }, this)
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 43,
        columnNumber: 9
      }, this)]
    }, void 0, true, {
      fileName: _jsxFileName,
      lineNumber: 24,
      columnNumber: 7
    }, this), openAtLeastOnce && /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_5__["default"], {
      hidden: !isOpen,
      ml: "9px",
      pl: "10px",
      borderLeft: "1px solid #DDD",
      children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_ContentFromUrl__WEBPACK_IMPORTED_MODULE_3__.ContentFromUrl, {
        url: contentUrl,
        modifyRendered: bodyNode => {
          var _bodyNode$props;

          return bodyNode == null ? void 0 : (_bodyNode$props = bodyNode.props) == null ? void 0 : _bodyNode$props.children;
        }
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 59,
        columnNumber: 11
      }, this)
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 57,
      columnNumber: 9
    }, this)]
  }, void 0, true, {
    fileName: _jsxFileName,
    lineNumber: 23,
    columnNumber: 5
  }, this);
}

/***/ }),

/***/ 81172:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "OverlayDialog": () => (/* binding */ OverlayDialog)
/* harmony export */ });
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(15735);
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(6886);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var _mui_icons_material__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(58568);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(69089);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(87037);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(79811);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(38677);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(2784);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var _ContentFromUrl__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(6404);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/libs/stex-react-renderer/src/lib/OverlayDialog.tsx";








function OverlayDialog({
  contentUrl,
  displayNode
}) {
  const [open, setOpen] = (0,react__WEBPACK_IMPORTED_MODULE_2__.useState)(false);
  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.Fragment, {
    children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)("div", {
      style: {
        display: "inline"
      },
      onClick: () => setOpen(true),
      children: displayNode
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 15,
      columnNumber: 7
    }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_5__["default"], {
      onClose: () => setOpen(false),
      open: open,
      maxWidth: "lg",
      children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)("a", {
        style: {
          marginLeft: "auto"
        },
        href: contentUrl,
        target: "_blank",
        rel: "noreferrer",
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_6__["default"], {
          children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_icons_material__WEBPACK_IMPORTED_MODULE_7__["default"], {}, void 0, false, {
            fileName: _jsxFileName,
            lineNumber: 21,
            columnNumber: 13
          }, this)
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 20,
          columnNumber: 11
        }, this)
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 19,
        columnNumber: 9
      }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_ContentFromUrl__WEBPACK_IMPORTED_MODULE_3__.ContentFromUrl, {
        url: contentUrl,
        modifyRendered: bodyNode => {
          var _bodyNode$props;

          return bodyNode == null ? void 0 : (_bodyNode$props = bodyNode.props) == null ? void 0 : _bodyNode$props.children;
        }
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 24,
        columnNumber: 9
      }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_8__["default"], {
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_4__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_9__["default"], {
          onClick: () => setOpen(false),
          children: "Close"
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 27,
          columnNumber: 11
        }, this)
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 26,
        columnNumber: 9
      }, this)]
    }, void 0, true, {
      fileName: _jsxFileName,
      lineNumber: 18,
      columnNumber: 7
    }, this)]
  }, void 0, true);
}

/***/ }),

/***/ 26290:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "SidebarButton": () => (/* binding */ SidebarButton)
/* harmony export */ });
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(15735);
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(6886);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var _mui_icons_material_InfoOutlined__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(89025);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(87037);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(18960);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(95341);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(2784);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/libs/stex-react-renderer/src/lib/SidebarButton.tsx";







function SidebarButton({
  sidebarContents
}) {
  // Menu crap start
  const [anchorEl, setAnchorEl] = (0,react__WEBPACK_IMPORTED_MODULE_2__.useState)(null);
  const open = Boolean(anchorEl);

  const handleClick = event => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  }; // Menu crap end


  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.Fragment, {
    children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_4__["default"], {
      onClick: handleClick,
      children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(_mui_icons_material_InfoOutlined__WEBPACK_IMPORTED_MODULE_5__["default"], {}, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 19,
        columnNumber: 9
      }, this)
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 18,
      columnNumber: 7
    }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_6__["default"], {
      id: "basic-menu",
      anchorEl: anchorEl,
      open: open,
      onClose: handleClose,
      MenuListProps: {
        "aria-labelledby": "basic-button"
      },
      children: sidebarContents.map(content => /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_3__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_7__["default"], {
        style: {
          fontSize: "small"
        },
        children: content
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 32,
        columnNumber: 11
      }, this))
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 22,
      columnNumber: 7
    }, this)]
  }, void 0, true);
}

/***/ }),

/***/ 95943:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "BASE_URL": () => (/* binding */ BASE_URL),
/* harmony export */   "HighlightContext": () => (/* binding */ HighlightContext),
/* harmony export */   "localStore": () => (/* binding */ localStore),
/* harmony export */   "mmtHTMLToReact": () => (/* binding */ mmtHTMLToReact)
/* harmony export */ });
/* harmony import */ var _home_jazzpirate_work_Software_sTeX_React_node_modules_babel_runtime_helpers_esm_objectWithoutPropertiesLoose_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(31461);
/* harmony import */ var core_js_modules_es_object_assign_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(43105);
/* harmony import */ var core_js_modules_es_object_assign_js__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_object_assign_js__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var core_js_modules_es_string_trim_js__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(45794);
/* harmony import */ var core_js_modules_es_string_trim_js__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_string_trim_js__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(15735);
/* harmony import */ var core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_3___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_array_iterator_js__WEBPACK_IMPORTED_MODULE_3__);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(6886);
/* harmony import */ var core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_4___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_web_dom_collections_iterator_js__WEBPACK_IMPORTED_MODULE_4__);
/* harmony import */ var core_js_modules_es_regexp_exec_js__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(77950);
/* harmony import */ var core_js_modules_es_regexp_exec_js__WEBPACK_IMPORTED_MODULE_5___default = /*#__PURE__*/__webpack_require__.n(core_js_modules_es_regexp_exec_js__WEBPACK_IMPORTED_MODULE_5__);
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_16__ = __webpack_require__(42921);
/* harmony import */ var _mui_material_styles__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(65992);
/* harmony import */ var _mui_material_Tooltip__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(23883);
/* harmony import */ var _mui_material_Tooltip__WEBPACK_IMPORTED_MODULE_15__ = __webpack_require__(43894);
/* harmony import */ var html_react_parser__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(86741);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(2784);
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_7___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_7__);
/* harmony import */ var _ContentFromUrl__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(6404);
/* harmony import */ var _ExpandableContent__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(74061);
/* harmony import */ var _OverlayDialog__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(81172);
/* harmony import */ var _SidebarButton__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(26290);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(49663);

const _excluded = ["className"];

var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/libs/stex-react-renderer/src/lib/mmtParser.tsx",
    _ref;

















const IS_SERVER = typeof window === 'undefined';
const BASE_URL = (_ref = IS_SERVER ? null : window.BASE_URL) != null ? _ref : 'https://overleaf.beta.vollki.kwarc.info';
const localStore = IS_SERVER ? undefined : localStorage;
const NoMaxWidthTooltip = (0,_mui_material_styles__WEBPACK_IMPORTED_MODULE_13__["default"])(_ref2 => {
  let {
    className
  } = _ref2,
      props = (0,_home_jazzpirate_work_Software_sTeX_React_node_modules_babel_runtime_helpers_esm_objectWithoutPropertiesLoose_js__WEBPACK_IMPORTED_MODULE_0__["default"])(_ref2, _excluded);

  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_mui_material_Tooltip__WEBPACK_IMPORTED_MODULE_14__["default"], Object.assign({}, props, {
    classes: {
      popper: className
    }
  }), void 0, false, {
    fileName: _jsxFileName,
    lineNumber: 22,
    columnNumber: 3
  }, undefined);
})({
  [`& .${_mui_material_Tooltip__WEBPACK_IMPORTED_MODULE_15__["default"].tooltip}`]: {
    maxWidth: 'none',
    backgroundColor: '#616161'
  }
});

function removeStyleTag(style, tag) {
  const start = style.indexOf(tag);
  if (start === -1) return style;
  const end = style.indexOf(';', start + 1);
  if (end === -1) return style;
  return style.substring(0, start) + style.substring(end + 1);
} // HACK: Return only the appropriate XHTML from MMT.


function getChildrenOfBodyNode(htmlNode) {
  var _htmlNode$props, _htmlNode$props$child, _body$props;

  const body = htmlNode == null ? void 0 : (_htmlNode$props = htmlNode.props) == null ? void 0 : (_htmlNode$props$child = _htmlNode$props.children) == null ? void 0 : _htmlNode$props$child[1];
  return body == null ? void 0 : (_body$props = body.props) == null ? void 0 : _body$props.children;
}

function isSidebar(node) {
  var _node$attribs;

  return (node == null ? void 0 : (_node$attribs = node.attribs) == null ? void 0 : _node$attribs['class']) === 'sidebar';
}

function isVisible(node) {
  var _data, _node$attribs2;

  if ((node == null ? void 0 : node.type) === 'text' && (node == null ? void 0 : (_data = node.data) == null ? void 0 : _data.trim().length) === 0) return false;
  const visibilityAttrib = node == null ? void 0 : (_node$attribs2 = node.attribs) == null ? void 0 : _node$attribs2['stex:visible'];
  return node && visibilityAttrib !== 'false';
}

function isLeafNode(node) {
  var _node$children;

  if (!node) return false;
  if (!((_node$children = node.children) != null && _node$children.length)) return true;
  return node.name === 'paragraph' || isSidebar(node);
}

function getFirstDisplayNode(node) {
  if (!node || !isVisible(node)) {
    return null;
  }

  if (isLeafNode(node)) return node;

  for (const child of node.children) {
    const first = getFirstDisplayNode(child);
    if (first) return first;
  }

  return null;
}

function getNextNode(domNode) {
  let nextAncestor = null;
  let current = domNode;

  while (!nextAncestor && current) {
    nextAncestor = current.nextSibling;

    while (nextAncestor && !isVisible(nextAncestor)) {
      nextAncestor = nextAncestor.nextSibling;
    }

    current = current.parent;
  }

  if (!nextAncestor) return null;
  return getFirstDisplayNode(nextAncestor);
}

function collectNeighbours(domNode) {
  const neighbours = [];
  let next = getNextNode(domNode);

  while (isSidebar(next)) {
    next.attribs['isattached'] = true;
    neighbours.push(next);
    next = getNextNode(next);
  }

  return neighbours;
}

const HighlightContext = /*#__PURE__*/(0,react__WEBPACK_IMPORTED_MODULE_7__.createContext)({
  highlightedParentId: '',
  setHighlightedParentId: _id => {
    /**/
  }
});

function Highlightable({
  highlightId,
  domNode
}) {
  const {
    highlightedParentId,
    setHighlightedParentId
  } = (0,react__WEBPACK_IMPORTED_MODULE_7__.useContext)(HighlightContext);
  const backgroundColor = highlightedParentId === highlightId ? 'yellow' : undefined;
  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)("span", {
    onMouseOver: () => setHighlightedParentId(highlightId),
    onMouseOut: () => setHighlightedParentId(''),
    style: {
      backgroundColor,
      cursor: 'pointer'
    },
    children: (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__.domToReact)([domNode], {
      replace
    })
  }, void 0, false, {
    fileName: _jsxFileName,
    lineNumber: 117,
    columnNumber: 5
  }, this);
}

const replace = (domNode, skipSidebar = false) => {
  var _previous$attribs, _domNode$attribs2, _domNode$attribs7;

  if (!(domNode instanceof html_react_parser__WEBPACK_IMPORTED_MODULE_6__.Element)) return;

  if (isSidebar(domNode)) {
    var _domNode$attribs;

    if (skipSidebar) return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.Fragment, {}, void 0, false);

    if ((_domNode$attribs = domNode.attribs) != null && _domNode$attribs['isattached']) {
      return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.Fragment, {}, void 0, false);
    }

    const neighbours = collectNeighbours(domNode);
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.Fragment, {
      children: [[domNode, ...neighbours].map((sidebarNode, idx) => /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_16__["default"], {
        height: "0px",
        maxWidth: "300px",
        display: {
          xs: 'none',
          md: 'block'
        },
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)("div", {
          className: "sidebarexpanded",
          style: {
            marginTop: `${idx * 40}px`
          },
          children: (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__.domToReact)(sidebarNode.children, {
            replace
          })
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 147,
          columnNumber: 13
        }, undefined)
      }, idx, false, {
        fileName: _jsxFileName,
        lineNumber: 141,
        columnNumber: 11
      }, undefined)), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_16__["default"], {
        height: "0px",
        display: {
          xs: 'block',
          md: 'none'
        },
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)("div", {
          className: "sidebarbuttonwrapper",
          children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_SidebarButton__WEBPACK_IMPORTED_MODULE_11__.SidebarButton, {
            sidebarContents: [domNode, ...neighbours].map(content => (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__.domToReact)(content.children, {
              replace
            }))
          }, void 0, false, {
            fileName: _jsxFileName,
            lineNumber: 158,
            columnNumber: 13
          }, undefined)
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 157,
          columnNumber: 11
        }, undefined)
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 156,
        columnNumber: 9
      }, undefined)]
    }, void 0, true);
  }

  if (!isVisible(domNode)) return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.Fragment, {}, void 0, false);
  const previous = domNode.previousSibling;

  if ((previous == null ? void 0 : (_previous$attribs = previous.attribs) == null ? void 0 : _previous$attribs.class) === 'sidebar' && ((_domNode$attribs2 = domNode.attribs) == null ? void 0 : _domNode$attribs2['class']) !== 'sidebar') {//if(domNode.attribs?.['style']) domNode.attribs['style']+=";margin-top: -20px;";
    //else domNode.attribs['style'] ="margin-top: -20px;";
  }

  if (domNode.name === 'head' || domNode.name === 'iframe' || domNode.name === 'script') {
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.Fragment, {}, void 0, false);
  }

  if (!(localStore != null && localStore.getItem('no-responsive'))) {
    var _domNode$attribs3, _domNode$attribs5;

    // HACK: Make MMT return appropriate (X)HTML for responsive page.
    if (((_domNode$attribs3 = domNode.attribs) == null ? void 0 : _domNode$attribs3['class']) === 'body') {
      var _domNode$attribs4;

      domNode.attribs['style'] = removeStyleTag(removeStyleTag((_domNode$attribs4 = domNode.attribs) == null ? void 0 : _domNode$attribs4['style'], 'padding-left'), 'padding-right');
    }

    if ((_domNode$attribs5 = domNode.attribs) != null && _domNode$attribs5['style']) {
      var _domNode$attribs6;

      domNode.attribs['style'] = removeStyleTag(removeStyleTag((_domNode$attribs6 = domNode.attribs) == null ? void 0 : _domNode$attribs6['style'], 'min-width'), 'width');
    }
  }

  const hoverLink = domNode.attribs['data-overlay-link-hover'];
  const clickLink = domNode.attribs['data-overlay-link-click'];
  const hoverParent = domNode.attribs['data-highlight-parent'];

  if ((hoverLink || clickLink) && !domNode.attribs['processed']) {
    domNode.attribs['processed'] = 'first';
    const tooltipPath = BASE_URL + hoverLink;
    const dialogPath = BASE_URL + clickLink; // eslint-disable-next-line react/display-name

    const WithHighlightable = /*#__PURE__*/(0,react__WEBPACK_IMPORTED_MODULE_7__.forwardRef)((props, ref) => {
      return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)("div", Object.assign({}, props, {
        style: {
          display: 'inline',
          cursor: 'pointer'
        },
        ref: ref,
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(Highlightable, {
          domNode: domNode,
          highlightId: hoverParent
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 218,
          columnNumber: 11
        }, undefined)
      }), void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 213,
        columnNumber: 9
      }, undefined);
    });
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_OverlayDialog__WEBPACK_IMPORTED_MODULE_10__.OverlayDialog, {
      contentUrl: dialogPath,
      displayNode: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(NoMaxWidthTooltip, {
        title: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)("div", {
          style: {
            minWidth: '300px',
            maxWidth: '600px'
          },
          children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_ContentFromUrl__WEBPACK_IMPORTED_MODULE_8__.ContentFromUrl, {
            url: tooltipPath,
            modifyRendered: n => getChildrenOfBodyNode(n)
          }, void 0, false, {
            fileName: _jsxFileName,
            lineNumber: 229,
            columnNumber: 17
          }, undefined)
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 228,
          columnNumber: 15
        }, undefined),
        children: hoverParent ? /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(WithHighlightable, {}, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 237,
          columnNumber: 15
        }, undefined) : (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__.domToReact)([domNode], {
          replace
        })
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 226,
        columnNumber: 11
      }, undefined)
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 223,
      columnNumber: 7
    }, undefined);
  }

  if (hoverParent && !domNode.attribs['processed']) {
    domNode.attribs['processed'] = 'second';
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(Highlightable, {
      domNode: domNode,
      highlightId: hoverParent
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 249,
      columnNumber: 12
    }, undefined);
  } // HACK: get url using attribs for the sidebar buttons.


  if (domNode.attribs['onclick']) {
    const rx = /stexMainOverlayOn\('(.*)'/g;
    const matches = rx.exec(domNode.attribs['onclick']);
    const path = BASE_URL + (matches == null ? void 0 : matches[1]);
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_OverlayDialog__WEBPACK_IMPORTED_MODULE_10__.OverlayDialog, {
      contentUrl: path,
      displayNode: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.Fragment, {
        children: (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__.domToReact)([domNode])
      }, void 0, false)
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 258,
      columnNumber: 7
    }, undefined);
  }

  if (((_domNode$attribs7 = domNode.attribs) == null ? void 0 : _domNode$attribs7['class']) === 'inputref') {
    const inputRef = domNode.attribs['data-inputref-url'];
    return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_12__.jsxDEV)(_ExpandableContent__WEBPACK_IMPORTED_MODULE_9__.ExpandableContent, {
      contentUrl: BASE_URL + inputRef,
      title: (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__.domToReact)(domNode.children, {
        replace
      })
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 267,
      columnNumber: 7
    }, undefined);
  }

  return;
};

function mmtHTMLToReact(html, skipSidebar = false) {
  return (0,html_react_parser__WEBPACK_IMPORTED_MODULE_6__["default"])(html, {
    replace: d => replace(d, skipSidebar)
  });
}

/***/ }),

/***/ 36911:
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "BG_COLOR": () => (/* binding */ BG_COLOR),
/* harmony export */   "ContentFromUrl": () => (/* reexport safe */ _ContentFromUrl__WEBPACK_IMPORTED_MODULE_0__.ContentFromUrl),
/* harmony export */   "StexReactRenderer": () => (/* binding */ StexReactRenderer),
/* harmony export */   "mmtHTMLToReact": () => (/* reexport safe */ _mmtParser__WEBPACK_IMPORTED_MODULE_1__.mmtHTMLToReact)
/* harmony export */ });
/* harmony import */ var _mui_material__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(42921);
/* harmony import */ var _ContentFromUrl__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(6404);
/* harmony import */ var _mmtParser__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(95943);
/* harmony import */ var react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(49663);
var _jsxFileName = "/home/jazzpirate/work/Software/sTeX-React/libs/stex-react-renderer/src/lib/stex-react-renderer.tsx";




const BG_COLOR = 'hsl(210, 20%, 98%)';
function StexReactRenderer({
  contentUrl
}) {
  return /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_2__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_3__["default"], {
    display: "flex",
    children: [/*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_2__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_3__["default"], {
      flexGrow: 1,
      flexBasis: 600,
      bgcolor: BG_COLOR,
      children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_2__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_3__["default"], {
        p: "0 10px 0 40px",
        maxWidth: "520px",
        m: "0 auto",
        bgcolor: BG_COLOR,
        children: /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_2__.jsxDEV)(_ContentFromUrl__WEBPACK_IMPORTED_MODULE_0__.ContentFromUrl, {
          url: contentUrl,
          modifyRendered: bodyNode => {
            var _bodyNode$props;

            return bodyNode == null ? void 0 : (_bodyNode$props = bodyNode.props) == null ? void 0 : _bodyNode$props.children;
          }
        }, void 0, false, {
          fileName: _jsxFileName,
          lineNumber: 12,
          columnNumber: 11
        }, this)
      }, void 0, false, {
        fileName: _jsxFileName,
        lineNumber: 11,
        columnNumber: 9
      }, this)
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 10,
      columnNumber: 7
    }, this), /*#__PURE__*/(0,react_jsx_dev_runtime__WEBPACK_IMPORTED_MODULE_2__.jsxDEV)(_mui_material__WEBPACK_IMPORTED_MODULE_3__["default"], {
      flexBasis: 300,
      display: {
        xs: 'none',
        md: 'block'
      },
      bgcolor: BG_COLOR
    }, void 0, false, {
      fileName: _jsxFileName,
      lineNumber: 18,
      columnNumber: 7
    }, this)]
  }, void 0, true, {
    fileName: _jsxFileName,
    lineNumber: 9,
    columnNumber: 5
  }, this);
}


/***/ })

},
/******/ __webpack_require__ => { // webpackRuntimeModules
/******/ var __webpack_exec__ = (moduleId) => (__webpack_require__(__webpack_require__.s = moduleId))
/******/ __webpack_require__.O(0, ["vendor"], () => (__webpack_exec__(61871)));
/******/ var __webpack_exports__ = __webpack_require__.O();
/******/ }
]);
//# sourceMappingURL=main.js.map