const x: number = 245;

type Coll<T> = Array<T> | Map<string, T>;

declare let c1: Coll<string>;
declare let c2: Array<number>;

// c1 = c2;

class Person {
  public name: string;
  public age: number;
}

const p1: Person = new Person();
p1.age = 30;

function foo(x: number): number {
  return x + 4;
}

function goo(person: Person): number {
  return foo(person.age) + foo(person.age);
}

let nop: any = goo({name: 'Konrad', age: 44});

const l1 = ['xyz', 23, 3.14] as const;

interface User {
  email: string;
  age?: number;
}

const u1: User = {
  email: 'kongra@gmail.com',
};

const u2 = {
  email: 'kongra@gmail.com',
} as const;


console.log(u1.age);