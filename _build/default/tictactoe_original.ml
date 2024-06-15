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

let setup () =
	init_window (get_screen_width ()) (get_screen_height ()) "Modern Tic Tac Toe";
	set_target_fps 60;
	begin_drawing ();
	clear_background (Color.create 44 44 49 1);
	end_drawing ();
	let board = [(Gray, Default); (Gray, Default); (Gray, Default);
					 (Gray, Default); (Gray, Default); (Gray, Default);
					 (Gray, Default); (Gray, Default); (Gray, Default)]
	in
	board
;;

let print_tile symbol_type symbol_color smallbox_color mediumbox_color largebox_color n =
	let wstart = (float (get_screen_width ())) /. 2. -. 225. in
	let hstart = (float (get_screen_height ())) /. 2. -. 225. in
	
	let x, y = match n with
	| 0 -> wstart, hstart
	| 1 -> wstart +. 150., hstart
	| 2 -> wstart +. 300., hstart
	| 3 -> wstart, hstart +. 150.
	| 4 -> wstart +. 150., hstart +. 150.
	| 5 -> wstart +. 300., hstart +. 150.
	| 6 -> wstart, hstart +. 300.
	| 7 -> wstart +. 150., hstart +. 300.
	| 8 -> wstart +. 300., hstart +. 300.
	| _ -> 0., 0.
	in
	begin_drawing ();
	draw_rectangle_rounded (Rectangle.create (x +. 40.) (y +. 40.) 110. 110.) 0.35 0 largebox_color;
	draw_rectangle_rounded (Rectangle.create (x +. 50.) (y +. 50.) 90. 90.) 0.3 0 mediumbox_color;
	draw_rectangle_rounded (Rectangle.create (x +. 60.) (y +. 60.) 70. 70.) 0.25 0 smallbox_color;
	
	if (symbol_type = "cross") then begin
		draw_rectangle_pro (Rectangle.create x y 50. 7.) (Vector2.create (-109.) 3.) 45. symbol_color;
		draw_rectangle_pro (Rectangle.create x y 7. 50.) (Vector2.create (-131.) 25.) 45. symbol_color
	end
	else begin
		draw_circle ((int_of_float x) + 95) ((int_of_float y) + 95) 22. symbol_color;
		draw_circle ((int_of_float x) + 95) ((int_of_float y) + 95) 14. smallbox_color
	end;
	end_drawing ()
;;

let rec list_change l n a b =
	match l with
	| [] -> []
	| t::q when n = 0 -> (a, b)::(list_change q (n-1) a b)
	| t::q -> t::(list_change q (n-1) a b)
;;

let rec what_tile l n =
	match l with
	| [] -> failwith "error what_tile"
	| t::q when n = 0 -> t
	| t::q -> what_tile q (n-1)
;;

let print_tile_colors board n a b =
	let dark_gray = Color.create 52 52 54 255 in
	let gray = Color.create 86 86 89 255 in
	let light_gray = Color.create 129 129 129 255 in
	let white = Color.create 218 218 218 255 in
	let red = Color.create 255 0 0 255 in
	let blue = Color.create 0 128 255 255 in
	let purple = Color.create 167 0 255 255 in

   let largebox_color =
   match a, b with
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
	match a, b with
	| Red, Default | Red, Winner -> white
	| Blue, Default | Blue, Winner -> white
	| Gray, Default | Gray, Highlighted -> largebox_color
	| Gray, _ -> dark_gray
	| _, _ -> largebox_color
	in
	let smallbox_color =
	match a, b with
	| Red, Winner -> red
	| Blue, Winner -> blue
	| Gray, Default -> gray
	| Gray, Highlighted -> light_gray
	| _ -> dark_gray
	in
	let mediumbox_color =
	match a, b with
	| Red, Default | Red, Winner -> white
	| Blue, Default | Blue, Winner -> white
	| _, _ -> largebox_color
	in
	let symbol_type =
	match a with
	| Red -> "cross"
	| _ -> "circle"
	in
	print_tile symbol_type symbol_color smallbox_color mediumbox_color largebox_color n;
	list_change board n a b
;;

let rec refresh_game square_color l =
	let wstart = (float (get_screen_width ())) /. 2. -. 225. in
	let hstart = (float (get_screen_height ())) /. 2. -. 225. in
	draw_rectangle_rounded (Rectangle.create wstart hstart 490. 490.) 0.1 0 square_color;
	refresh_game_aux l l 0
and refresh_game_aux board l n =
	match l, n with
	| (a, b)::q, _ when n < 9 -> refresh_game_aux (print_tile_colors board n a b) q (n+1)
	| _, _ -> ()
;;

let is_valid_click () =
	let x = get_mouse_x () in
	let y = get_mouse_y () in
	let wstart = (get_screen_width ()) / 2 - 205 in
	let hstart = (get_screen_height ()) / 2 - 205 in
	if (wstart < x && x < (wstart + 450)) && (hstart < y && y < (hstart + 450)) then
		true
	else
		false
;;

let clicked_tile () =
   if is_valid_click () then begin
	   let tile_x, tile_y =
			( ((get_mouse_x ()) - ((get_screen_width ()) / 2 - 205)) / 150), ( ((get_mouse_y ()) - ((get_screen_height ()) / 2 - 205)) / 150)
		in
		(3*(tile_y) + tile_x)
	end
	else -1
;;

let reset_game board player =
	board := [(Gray, Default); (Gray, Default); (Gray, Default);
				 (Gray, Default); (Gray, Default); (Gray, Default);
				 (Gray, Default); (Gray, Default); (Gray, Default)];
	player := Red;
	refresh_game (Color.create 218 218 218 255) !board
;;

let win_test l n1 n2 n3 =
	(what_tile l n1) = (what_tile l n2) &&
	(what_tile l n2) = (what_tile l n3) &&
	((what_tile l n1 = (Red, Default)) || (what_tile l n1 = (Blue, Default)))
;;

let winning_tiles_test l player n1 n2 n3 =
	match win_test l n1 n2 n3, what_tile l n1 with
	| true, (tile_player, Default) -> if tile_player = player then [n1; n2; n3] else []
	| _ -> []



let rec print_list l =
	match l with
	|[] -> print_string "[]"
	|t::q -> print_int t; print_string "::"; print_list q
;;





let winning_tiles l player =
	let l_win_tiles =
	(winning_tiles_test l player 0 1 2) @
	(winning_tiles_test l player 3 4 5) @
	(winning_tiles_test l player 6 7 8) @
	(winning_tiles_test l player 0 3 6) @
	(winning_tiles_test l player 1 4 7) @
	(winning_tiles_test l player 2 5 8) @
	(winning_tiles_test l player 0 4 8) @
	(winning_tiles_test l player 2 4 6) @ []
	in
	l_win_tiles
;;

let rec no_win l =
	match l with
	| [] -> true
	| (Gray, Default)::q -> false
	| t::q -> no_win q
;;

let rec is_elt_in l x =
	match l with
	| [] -> false
	| t::q -> if x = t then true else is_elt_in q x
;;

let rec end_board l winner l_win_tiles n =
	let new_board = ref l in
	for i = 0 to 8 do
		if true then
			new_board := list_change l i Blue Winner
		else
			new_board := list_change l i Red Red_win
	done;
	!new_board
;;

let win newboard ended =
	let l_win_tiles_red = winning_tiles !newboard Red in
	let l_win_tiles_blue = winning_tiles !newboard Blue in
	if no_win !newboard then begin
		newboard := end_board !newboard No_win [] 0;
		refresh_game (Color.create 167 0 255 255) !newboard;
		print_list l_win_tiles_red;
		ended := true;
	end
	else if l_win_tiles_red <> [] then begin
		newboard := end_board !newboard Red_win l_win_tiles_red 0;
		refresh_game (Color.create 255 0 0 255) !newboard;
		print_list l_win_tiles_red;
		ended := true;
	end
	else if l_win_tiles_blue <> [] then begin
		newboard := end_board !newboard Blue_win l_win_tiles_blue 0;
		refresh_game (Color.create 0 128 255 255) !newboard;
		ended := true;
	end;
	ended, newboard
;;







let game board player old_click_state ended =
	let new_player = ref player in
	let new_board = ref board in
	let n = clicked_tile () in

	let pressed = (is_mouse_button_down Left) in
	if ( n <> -1) && (old_click_state <> pressed) && (pressed = false) then begin
		if (what_tile board n = (Gray, Default)) then
			new_board := print_tile_colors board n player Default;
		new_player :=
      match !new_player with
		| Red -> Blue
		| _ -> Red
   end;

	if is_key_down Space then begin
		reset_game new_board new_player;
		ended := false
	end;

	if (not !ended) then begin
		let a, b = (win new_board ended) in
		ended := !a;
		new_board := !b;
	end;

	begin_drawing ();
	end_drawing ();
	!new_board, !new_player, pressed, ended
;;

let rec loop (board, player, click_state, ended) =
	match window_should_close () with
	| true -> close_window ()
	| false ->
		loop (game board player click_state ended)
;;

let () =
	let board = setup () in
	refresh_game (Color.create 218 218 218 255) board;
	loop (board, Red, false, ref false)
;;

