(* Auto-generated from "transactions.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type transaction = Transactions_t.transaction = {
  action: string;
  stock: string;
  volume: int;
  price: float;
  time: string
}

let write_transaction : _ -> transaction -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"action\":";
    (
      Yojson.Safe.write_string
    )
      ob x.action;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"stock\":";
    (
      Yojson.Safe.write_string
    )
      ob x.stock;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"volume\":";
    (
      Yojson.Safe.write_int
    )
      ob x.volume;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"price\":";
    (
      Yojson.Safe.write_float
    )
      ob x.price;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"time\":";
    (
      Yojson.Safe.write_string
    )
      ob x.time;
    Bi_outbuf.add_char ob '}';
)
let string_of_transaction ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_transaction ob x;
  Bi_outbuf.contents ob
let read_transaction = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_action = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_stock = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_volume = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_price = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_time = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  4
                )
                else (
                  -1
                )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'k' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 6 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 'v' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_action := (
              (
                Atdgen_runtime.Oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_stock := (
              (
                Atdgen_runtime.Oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_volume := (
              (
                Atdgen_runtime.Oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_price := (
              (
                Atdgen_runtime.Oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_time := (
              (
                Atdgen_runtime.Oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'k' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 6 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'n' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 'v' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_action := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_stock := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_volume := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_price := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_time := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1f then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "action"; "stock"; "volume"; "price"; "time" |];
        (
          {
            action = !field_action;
            stock = !field_stock;
            volume = !field_volume;
            price = !field_price;
            time = !field_time;
          }
         : transaction)
      )
)
let transaction_of_string s =
  read_transaction (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
