open Raylib

type t = {
  scale: int;
  width: int;
  height: int;
}

let create ~x_count ~y_count ~scale =
  {scale; width = scale * x_count; height = scale * y_count}

let get_scaled_mouse_position ~scale =
  let absolute_position = get_mouse_position () in
  (((Vector2.x absolute_position) |> int_of_float) / scale,
    ((Vector2.y absolute_position) |> int_of_float) / scale)
