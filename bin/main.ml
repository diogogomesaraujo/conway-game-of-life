open Raylib
open Game_of_life

let window = Window.create ~x_count:40 ~y_count:30 ~scale:10

let setup () =
  init_window window.width window.height "Conway's Game of Life";
  set_target_fps 60;
  let game_state = State.create ~width:window.width ~height:window.height~scale:window.scale in
  game_state

let rec loop state =
  if window_should_close () then Raylib.close_window ()
  else
    let state = State.after_user_input ~state:state ~scale:window.scale in
    let state =
      match is_key_pressed Key.Space with
      | true -> State.after_frame ~state:state
      | false -> state
    in
    begin_drawing ();
    clear_background Color.black;
    State.draw_alive_cells ~state:state ~alive_cell_color:Color.white;
    end_drawing ();
    loop state

let () =
  let state = setup () in
  loop state
