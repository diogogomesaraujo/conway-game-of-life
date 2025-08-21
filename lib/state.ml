open Raylib

module Cell = struct
  type state =
    | Alive
    | Dead

  type t = {
    state: state;
    body: Rectangle.t;
  }

  let state ~cell = cell.state

  let body ~cell = cell.body

  let opposite_state ~cell =
    match cell.state with
    | Alive -> {cell with state = Dead}
    | Dead -> {cell with state = Alive}

  let create ~x ~y ~size =
    {state = Dead; body = Rectangle.create x y size size}

  let draw ~cell ~color =
    draw_rectangle_rec cell.body color

end

type t = {
  width: int;
  height: int;
  cells: Cell.t array array;
}

let create ~width ~height ~scale =
  let cells =
    Array.init height (fun w ->
      Array.init width (fun h ->
        Cell.create
          ~x: (w * scale |> float_of_int)
          ~y: (h * scale |> float_of_int)
          ~size: (scale |> float_of_int)
      )
    )
  in
  {width; height; cells}

let draw_alive_cells ~state ~alive_cell_color =
  Array.iter (fun row ->
    Array.iter (fun e ->
      match Cell.state ~cell:e with
      | Cell.Alive -> Cell.draw ~cell:e ~color:alive_cell_color
      | _ -> ()
    ) row
  ) state.cells

let after_user_input ~state ~scale =
  match is_mouse_button_pressed MouseButton.Left with
  | true ->
    let (x, y) = Window.get_scaled_mouse_position ~scale:scale in
    state.cells.(x).(y)
      <- Cell.opposite_state
      ~cell:state.cells.(x).(y);
    state
  | _ -> state

let after_frame ~state =
  let possible_directions = [
    (0, 1); (1, 0);
    (1, 1); (0, -1);
    (-1, 0); (-1, -1);
    (-1, 1); (1, -1)
  ] in

  let check_alive ~cell_x ~cell_y ~(cells : Cell.t array array) =
    let len_x = Array.length cells in
    let len_y = Array.length cells.(0) in
    let alive_neighbours_count = List.fold_left (
      fun acc dir ->
        let (dir_x, dir_y) = dir in
        let (neighbour_x, neighbour_y) = (cell_x + dir_x, cell_y + dir_y) in
        match (neighbour_x, neighbour_y) with
        | (x, _) when x >= len_x || x < 0 -> acc
        | (_, y) when y >= len_y || y < 0 -> acc
        | _ ->
          match cells.(neighbour_x).(neighbour_y).state with
          | Cell.Alive -> acc + 1
          | Cell.Dead -> acc
    ) 0 possible_directions in

    match (alive_neighbours_count, cells.(cell_x).(cell_y).state) with
    | (count, Cell.Alive) when count = 2 || count = 3 -> true
    | (count, Cell.Dead) when count = 3 -> true
    | _ -> false
  in

  {state with cells = (Array.mapi (
    fun x row -> Array.mapi (
      fun y cell ->
        match (check_alive ~cell_x:x ~cell_y:y ~cells:state.cells, Cell.state ~cell:cell) with
        | (false, Cell.Alive) -> Cell.opposite_state ~cell:cell
        | (true, Cell.Dead) -> Cell.opposite_state ~cell:cell
        | _ -> cell
    ) row
  ) state.cells)}
