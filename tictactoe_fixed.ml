open Raylib

type tile_color =
	| Red
	| Blue
	| Gray
;;

type tile_mode =
	| Default
	| Highlighted
	| Red_win
	| Blue_win
	| No_win
	| Winner
;;



(*global values*)
let player = ref Red;;
let board_color = ref (Color.create 218 218 218 255);;
let game_ended = ref false;;



(*raylib functions*)
let window_init () =
	init_window 0 0 "Clean Tic Tac Toe";
	clear_background (Color.create 44 44 49 1);
	set_target_fps 60;
;;

let print_tile symbol_type symbol_color smallbox_color mediumbox_color largebox_color x y =
	let wstart = (float (get_screen_width ())) /. 2. -. 225. in
	let hstart = (float (get_screen_height ())) /. 2. -. 225. in
	let screen_x, screen_y = wstart +. 150. *. x, hstart +. 150. *. y in

	draw_rectangle_rounded (Rectangle.create (screen_x +. 40.) (screen_y +. 40.) 110. 110.) 0.35 0 largebox_color;
	draw_rectangle_rounded (Rectangle.create (screen_x +. 50.) (screen_y +. 50.) 90. 90.) 0.3 0 mediumbox_color;
	draw_rectangle_rounded (Rectangle.create (screen_x +. 60.) (screen_y +. 60.) 70. 70.) 0.25 0 smallbox_color;
	
	if (symbol_type = "cross") then begin
		draw_rectangle_pro (Rectangle.create screen_x screen_y 50. 7.) (Vector2.create (-109.) 3.) 45. symbol_color;
		draw_rectangle_pro (Rectangle.create screen_x screen_y 7. 50.) (Vector2.create (-131.) 25.) 45. symbol_color
	end
	else begin
		draw_circle ((int_of_float screen_x) + 95) ((int_of_float screen_y) + 95) 22. symbol_color;
		draw_circle ((int_of_float screen_x) + 95) ((int_of_float screen_y) + 95) 14. smallbox_color
	end
;;

let set_tile_colors (color, mode) x y =
	let dark_gray = Color.create 52 52 54 255 in
	let gray = Color.create 86 86 89 255 in
	let light_gray = Color.create 129 129 129 255 in
	let white = Color.create 218 218 218 255 in
	let red = Color.create 255 0 0 255 in
	let blue = Color.create 0 128 255 255 in
	let purple = Color.create 167 0 255 255 in

   let largebox_color =
   match color, mode with
   | Red, Default | Red, Winner -> red
   | Blue, Default | Blue, Winner-> blue
   | Gray, Default -> gray
   | Gray, Highlighted -> light_gray
   | _, Red_win -> red
   | _, Blue_win -> blue
   | _, No_win -> purple
   | _, _ -> white
   in
	let symbol_color =
	match color, mode with
	| Red, Default | Red, Winner -> white
	| Blue, Default | Blue, Winner -> white
	| Gray, Default | Gray, Highlighted -> largebox_color
	| Gray, _ -> dark_gray
	| _, _ -> largebox_color
	in
	let smallbox_color =
	match color, mode with
	| Red, Winner -> red
	| Blue, Winner -> blue
	| Gray, Default -> gray
	| Gray, Highlighted -> light_gray
	| _ -> dark_gray
	in
	let mediumbox_color =
	match color, mode with
	| Red, Default | Red, Winner -> white
	| Blue, Default | Blue, Winner -> white
	| _, _ -> largebox_color
	in
	let symbol_type =
	match color with
	| Red -> "cross"
	| _ -> "circle"
	in
	print_tile symbol_type symbol_color smallbox_color mediumbox_color largebox_color (float_of_int x) (float_of_int y)
;;

let draw_game board =
	let wstart = (float_of_int (get_screen_width ())) /. 2. -. 225. in
	let hstart = (float_of_int (get_screen_height ())) /. 2. -. 225. in
	clear_background (Color.create 44 44 49 1);
	let outline_color = match !game_ended, !player with
		|false, Red -> Color.create 255 0 0 255
		|false, _ -> Color.create 0 128 255 255
		|_, _ -> !board_color
	in
	draw_rectangle_rounded (Rectangle.create (wstart -. 10.) (hstart -. 10.) 510. 510.) 0.13 0 outline_color;
	draw_rectangle_rounded (Rectangle.create wstart hstart 490. 490.) 0.1 0 !board_color;
	for i = 0 to 8 do
		set_tile_colors board.(i) (i mod 3) (i / 3)
	done;
	if !game_ended then draw_text "Press SPACE to restart" (int_of_float (wstart -. 10.)) (int_of_float (hstart +. 550.)) 40 Color.white
;;



(*functions*)
let is_valid_click x y w h =
	let wstart = w / 2 - 205 in
	let hstart = h / 2 - 205 in
	(wstart < x && x < (wstart + 450)) && (hstart < y && y < (hstart + 450))
;;

let what_tile board x y =
	let w, h = get_screen_width (), get_screen_height () in
   if is_valid_click x y w h then begin
	   let tile_x, tile_y = ((x - (w / 2 - 205)) / 150), ((y - (h / 2 - 205)) / 150) in
		let nb = (3*(tile_y) + tile_x) in
		if board.(nb) = (Gray, Default) then board.(nb) <- fst board.(nb), Highlighted;
		nb
	end
	else -1
;;

let color_tiles_win board win_type =
	for i = 0 to 8 do
		let tile, mode = board.(i) in
		if mode <> Winner then board.(i) <- tile, win_type
	done
;;

let is_line_winning current_winner board n1 n2 n3 = (*checks if specified tiles are the same and if yes changes their color*)
	if fst board.(n1) = fst board.(n2) && fst board.(n2) = fst board.(n3) && fst board.(n1) <> Gray then begin
		let color = fst board.(n1) in
		board.(n1) <- color, Winner;
		board.(n2) <- color, Winner;
		board.(n3) <- color, Winner;
		fst board.(n1)
	end
	else current_winner
;;

let check_win board =
	let winner = ref Gray in
	winner := is_line_winning !winner board 0 1 2;
	winner := is_line_winning !winner board 3 4 5;
	winner := is_line_winning !winner board 6 7 8;
	winner := is_line_winning !winner board 0 3 6;
	winner := is_line_winning !winner board 1 4 7;
	winner := is_line_winning !winner board 2 5 8;
	winner := is_line_winning !winner board 0 4 8;
	winner := is_line_winning !winner board 2 4 6;

	if !winner = Red then begin
		game_ended := true;
		color_tiles_win board Red_win;
		board_color := Color.create 255 0 0 255
	end
	else if !winner = Blue then begin
		game_ended := true;
		color_tiles_win board Blue_win;
		board_color := Color.create 0 128 255 255
	end
;;

let check_no_win board =
	let no_win = ref true in
	for i = 0 to 8 do
		if fst board.(i) = Gray then no_win := false;
		if snd board.(i) = Winner then no_win := false
	done;
	if !no_win then begin
		game_ended := true;
		board_color := Color.create 167 0 255 255;
		for i = 0 to 8 do
			board.(i) <- fst board.(i), No_win
		done
	end
;;

let rec loop board =
	match window_should_close () with
	| true -> close_window ()
	| false ->
		for i = 0 to 8 do
			if board.(i) = (Gray, Highlighted) then board.(i) <- (Gray, Default)
		done;
		let x, y = get_mouse_x (), get_mouse_y () in
		let tile_nb = what_tile board x y in
		if is_mouse_button_pressed Left then begin
			if tile_nb <> -1 && board.(tile_nb) = (Gray, Highlighted) then begin
				board.(tile_nb) <- (!player, Default);
				check_win board;
				check_no_win board;
				player := match !player with |Red -> Blue |Blue -> Red
			end
		end;
		if is_key_pressed Space && !game_ended then begin
			board_color := Color.create 218 218 218 255;
			game_ended := false;
			for i = 0 to 8 do
				board.(i) <- (Gray, Default)
			done
		end;
		draw_game board;
		begin_drawing ();
		end_drawing ();
		loop board
;;

let _ =
	window_init ();
	let board = Array.make 9 (Gray, Default) in
	loop board
;;
