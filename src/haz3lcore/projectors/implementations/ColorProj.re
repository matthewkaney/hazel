open Util;
open OptUtil.Syntax;
open Virtual_dom.Vdom;
open ProjectorBase;

type vec3 = (int, int, int);

type big_vec3 = (Bigint.t, Bigint.t, Bigint.t);

module Color = {
  type t = vec3;

  let clamp = (x: Bigint.t): int =>
    // This will never throw because the value is in the range 0-255
    Bigint.to_int_exn(
      Bigint.max(Bigint.of_int(0), Bigint.min(Bigint.of_int(255), x)),
    );

  let of_bigint = ((r, g, b): big_vec3): t => (
    clamp(r),
    clamp(g),
    clamp(b),
  );

  let to_bigint = ((r, g, b): t): big_vec3 => (
    Bigint.of_int(r),
    Bigint.of_int(g),
    Bigint.of_int(b),
  );

  let to_hex = ((r, g, b): t) => Printf.sprintf("#%02x%02x%02x", r, g, b);

  let from_hex = (hex: string): option(t) =>
    if (String.length(hex) == 7 && hex.[0] == '#') {
      let* r = int_of_string_opt("0x" ++ String.sub(hex, 1, 2));
      let* g = int_of_string_opt("0x" ++ String.sub(hex, 3, 2));
      let+ b = int_of_string_opt("0x" ++ String.sub(hex, 5, 2));
      (r, g, b);
    } else {
      None;
    };
};

module Syntax = {
  let exp_to_vec3 = (term: Language.Exp.t): option(big_vec3) =>
    switch (term.term) {
    | Tuple([
        {term: Atom(Int(r)), _},
        {term: Atom(Int(g)), _},
        {term: Atom(Int(b)), _},
      ]) =>
      Some((r, g, b))
    | _ => None
    };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let big_vec3_of = (any: Language.Any.t): option(big_vec3) =>
    switch (any) {
    | Exp({term: Ap(_, {term: Constructor("RGB", _), _}, val_exp), _}) =>
      Syntax.exp_to_vec3(val_exp)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (big_vec3_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let get_hex = (info: info): string =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(big_vec3_of)
    ) {
    | Some(rgb) => rgb |> Color.of_bigint |> Color.to_hex
    | None => failwith("Slider: Get: not integer literal")
    };

  let put_hex = (info: info, v: string): Base.segment => {
    let (r, g, b) =
      switch (v |> Color.from_hex) {
      | Some(vec) => vec |> Color.to_bigint
      | None => failwith("ColorPicker: Put: invalid hex string " ++ v)
      };
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(
            {
              term:
                Ap(
                  dir,
                  ctor,
                  {term: Tuple([r_exp, g_exp, b_exp]), _} as tup,
                ),
              _,
            } as t,
          ) =>
          Exp({
            ...t,
            term:
              Ap(
                dir,
                ctor,
                {
                  ...tup,
                  term:
                    Tuple([
                      {
                        ...r_exp,
                        term: Atom(Int(r)),
                      },
                      {
                        ...g_exp,
                        term: Atom(Int(g)),
                      },
                      {
                        ...b_exp,
                        term: Atom(Int(b)),
                      },
                    ]),
                },
              ),
          })
        | _ => failwith("ColorPicker: Put: not RGB constructor"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("ColorPicker: Put: lift failed")
    };
  };

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(6);
  let update = (model, _, _) => model;

  let view = ({info, parent, _}: View.args(model, action)) => {
    let value = info |> get_hex;
    View.mk(
      Node.input(
        ~attrs=[
          Attr.on_input((_, v) => parent(SetSyntax(put_hex(info, v)))),
          Attr.create("type", "color"),
          Attr.string_property("value", value),
        ],
        (),
      ),
    );
  };
};
