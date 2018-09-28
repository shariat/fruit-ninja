open Reprocessing;

type fruitT = {
  pos: (float, float),
  velocity: (float, float),
  image: imageT,
};

type stateT = {
  fruits: list(fruitT),
  explosionImage: imageT,
};

let fruitNames = ["apple", "banana", "coconut", "orange", "watermelon"];

let explosion = "explosion";

let generateFruit = (name, ~env) => {
  pos: (
    Random.float(60.) +. 150.,
    Random.float(6.) +. float_of_int(Env.height(env)),
  ),
  image: Draw.loadImage(~filename="./assets/" ++ name ++ "_small.png", env),
  velocity: (Random.float(200.), Random.float(-600.)),
};

let generateFruits = env => List.map(generateFruit(~env), fruitNames);

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  {
    fruits: generateFruits(env),
    explosionImage:
      Draw.loadImage(~filename="./assets/explosion_small.png", env),
  };
};

let g = 300.;

let int_of_pos = ((x, y)) => (int_of_float(x), int_of_float(y));
let float_of_pos = ((x, y)) => (float_of_int(x), float_of_int(y));
let shift_of_pos = ((x, y)) => (x - 32, y - 32);
let applyAccl = ((x, y), g, env) => (x, y +. Env.deltaTime(env) *. g);

let updateFruit = ({pos: (x, y), velocity: (dx, dy)} as fruit, ~g, ~env) => {
  ...fruit,
  velocity: applyAccl((dx, dy), g, env),
  pos: (x +. Env.deltaTime(env) *. dx, y +. Env.deltaTime(env) *. dy),
};

let isInBounds = ({pos: (x, y)}, env) =>
  x < float_of_int(Env.width(env))
  && x >= 0.
  && y < float_of_int(Env.height(env));

let isUnderMouse = ({pos}, env) =>
  Utils.distf(~p1=float_of_pos(Env.mouse(env)), ~p2=pos) < 30.;

let turnIntoExplosion = (fruit, env, image) =>
  if (isUnderMouse(fruit, env)) {
    {...fruit, image};
  } else {
    fruit;
  };

let draw = (state, env) => {
  let state =
    if (Env.key(Space, env)) {
      setup(env);
    } else {
      state;
    };

  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  /* let delta = Env.deltaTime(env); */

  let fruits =
    List.map(updateFruit(~g, ~env), state.fruits)
    |> List.filter(a => isInBounds(a, env))
    |> List.map(a => turnIntoExplosion(a, env, state.explosionImage));

  let fruits =
    if (List.length(fruits) < 3) {
      [
        generateFruit(
          List.nth(fruitNames, Random.int(List.length(fruitNames))),
          ~env,
        ),
        ...fruits,
      ];
    } else {
      fruits;
    };

  let state = {...state, fruits};

  List.iter(
    a =>
      Draw.image(
        a.image,
        ~pos=a.pos |> int_of_pos |> shift_of_pos,
        ~width=64,
        ~height=64,
        env,
      ),
    state.fruits,
  );
  state;
};

run(~setup, ~draw, ());