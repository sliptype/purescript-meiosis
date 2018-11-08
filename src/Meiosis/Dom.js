'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

exports.createSubjectDriver = undefined;

exports.createActionCreator = function createActionCreator(action$) {
  return function (action) {
    return function (event) {
      return function () {
        return action$.next(action);
      };
    };
  };
};
