import rxjs from 'rxjs';

const mapObject = (proj, o) => Object
  .entries(o)
  .reduce((result, [name, value]) => ({
    ...result,
    [name]: proj(name, value),
  }), {});


export const run = (main) => (drivers) => () => {

  /*
    Create fake sources streams
    for each driver
  */
  const fakeSources = mapObject(
    _ => new rxjs.Subject(),
    drivers
  );

  /*
    Pass the fake sources into main
    to receive real sink streams
  */
  const sinks = main(fakeSources);

  /*
    Pass the sink streams into
    their drivers to
    receive real source streams
  */
  const sources = mapObject(
    name => drivers[name](sinks[name]),
    sinks
  );

  /*
    Make the fake source streams
    behave like the real source streams
  */
  mapObject(
    (name, source) => source.subscribe(fakeSources[name]),
    sources
  );

};

export const createSubjectDriver = (driver) => (...args) => {
  driver(...args);
  return new rxjs.Subject();
};

export const createActionCreator = (action$) => (action) => (event) => {
  return action$.next(action);
};
