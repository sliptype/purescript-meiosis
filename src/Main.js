'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.createActionCreator = exports.createSubjectDriver = exports.run = undefined;

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _rxjs = require('rxjs');

var _snabbdom = require('snabbdom');

var _toVNode = require('snabbdom/tovnode').default;

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var patch = _snabbdom.init([
  require('snabbdom/modules/class').default, // makes it easy to toggle classes
  require('snabbdom/modules/props').default, // for setting properties on DOM elements
  require('snabbdom/modules/style').default, // handles styling on elements with support for animations
  require('snabbdom/modules/eventlisteners').default, // attaches event listeners
]);

var mapObject = function mapObject(proj, o) {
  return Object.entries(o).reduce(function (result, _ref) {
    var _ref2 = _slicedToArray(_ref, 2),
        name = _ref2[0],
        value = _ref2[1];

    return _extends({}, result, _defineProperty({}, name, proj(name, value)));
  }, {});
};

exports.run = function run(main) {
  return function (drivers) {
    return function () {

      /*
        Create fake sources streams
        for each driver
      */
      var fakeSources = mapObject(function (_) {
        return new _rxjs.Subject();
      }, drivers);

      /*
        Pass the fake sources into main
        to receive real sink streams
      */
      var sinks = main(fakeSources);

      /*
        Pass the sink streams into
        their drivers to
        receive real source streams
      */
      var sources = mapObject(function (name) {
        return drivers[name](sinks[name]);
      }, sinks);

      /*
        Make the fake source streams
        behave like the real source streams
      */
      mapObject(function (name, source) {
        return source.subscribe(fakeSources[name]);
      }, sources);

    };
  };
};

function createSubjectDriver(driver) {
  return function () {
    driver.apply(undefined, arguments);
    return new _rxjs.Subject();
  };
};

exports.createSubjectDriver = createSubjectDriver;

exports.createDomDriver = function createDomDriver(selector) {
  const element = document.querySelector(selector);

  return createSubjectDriver(function (view$) {
    view$
      .scan(function (acc, curr) {
        return patch(acc, curr);
      }, _toVNode(element))
      .subscribe();
  });
};

exports.createActionCreator = function createActionCreator(action$) {
  return function (action) {
    return function (event) {
      return function () {
        return action$.next(action);
      };
    };
  };
};
