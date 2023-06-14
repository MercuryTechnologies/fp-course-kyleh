type Err = string

type Validation<A> = {
  match: <T> (
    onError: (err: Err) => T,
    onValue: (value: A) => T
  ) => T
}

function _Error<A>(err: Err): Validation<A> {
  return {
    match: (onError, _) => onError(err)
  }
}

function Value<A>(value: A): Validation<A> {
  return {
    match: (_, onValue) => onValue(value)
  }
}

function isError<A>(v: Validation<A>): boolean {
  return v.match(() => true, () => false);
}

const err = _Error<string>("test");
const val = Value<string>("value");

console.log(isError(err));
console.log(isError(val));
