open Printf
open String
open Pervasives
open List
open Random
open Thread
open Condition
open Mutex
open Sys



type player = 
    {
        username : string;
        position : float list ref;
        vect_vitesse : float list ref;
        direction : float ref;
        commande : float list ref;
        score :  int ref;
        inchan : in_channel;
        outchan : out_channel;
    };;


let players = ref [];;
let obstacles = ref [];;
Random.init (int_of_float (100000.0*.(Sys.time())));;
let goal = ref [(Random.float 800.0)-.400.0; (Random.float 600.0)-.300.0];;
let obj_radius = 10.0;;
let ve_radius = 15.0;;
let ob_radius = 15.0;;
let phase = ref "attente";;
let nb_clients = ref 0;;
let server_tickrate = ref 4.0;;
let server_refresh_tickrate = ref 0.300;;
let max_speed = 5.0;;
let win_cap = 1;;
let mut = Mutex.create();;
let mut1 = Mutex.create();;
let cond = Condition.create();;
let first_cnx = Condition.create();;
let reset = ref true;;
let sessions_end = ref false;;


let send_to_all players msg username =
    for i=0 to (List.length !players)-1 do
        let p = (List.nth !players i) in
        if p.username!=username then
            output_string p.outchan msg;
            flush p.outchan 
    done;;


let split_coord coord = 
    let c = List.nth (String.split_on_char 'X' coord) 1 in
    let c = String.split_on_char 'Y' c in
    let c = List.map (function x -> float_of_string x) c in
    c;;


let goal_string goal =
    let goal_str = "X"^(string_of_float (List.nth !goal 0))^"Y"^(string_of_float (List.nth !goal 1)) in
    goal_str;;

let get_by_username list username = 
    let p = ref (List.nth !list 0) in
    try 
        for i=0 to (List.length !list)-1 do
            let l = List.nth !list i in 
                if l.username=username then begin
                    p := l;
                    raise Exit
                end 
        done;
        !p
    with Exit -> !p;;

let get_index_by_inchan list inchan = 
    let p = ref 0 in
    (* try  *)
        for i=0 to (List.length !list)-1 do
        let l = List.nth !list i in 
        if l.inchan=inchan then
            p := i;
            (* raise Exit *)
        done;
        !p;;
    (* with Exit -> !p;; *)
    

let delete_user new_list players pl = 
    for i=0 to (List.length !players)-1 do
        let user = List.nth !players i in 
            if not(user=pl) then
                new_list := List.append !new_list [user]
    done;
    !new_list;;


let user_exists list username = 
    let res = ref false in 
    for i=0 to (List.length !list)-1 do
        let l = List.nth !list i in 
        if l.username=username then begin
            res := true
        end
    done;
    !res;;

let get_coords players =
    let coords = ref "" in 
    for i=0 to (List.length players)-1 do 
        let p = List.nth players i in 
        let x=string_of_float(List.nth !(p.position) 0) and y=string_of_float(List.nth !(p.position) 1) in
        if i=0 then
            coords := (p.username^":X"^x^"Y"^y)
        else
            coords := (!coords^"|"^p.username^":X"^x^"Y"^y)
    done;
    !coords;;


let get_vcoords players =
    let coords = ref "" in 
    for i=0 to (List.length players)-1 do 
        let p = List.nth players i in 
        let x=string_of_float(List.nth !(p.position) 0) and y=string_of_float(List.nth !(p.position) 1) in
        let vx=string_of_float(List.nth !(p.vect_vitesse) 0) and vy=string_of_float(List.nth !(p.vect_vitesse) 1) in
        let t = string_of_float !(p.direction) in 
            if i=0 then
                coords := (p.username^":X"^x^"Y"^y^"VX"^vx^"VY"^vy^"T"^t)
            else
                coords := (!coords^"|"^p.username^":X"^x^"Y"^y^"VX"^vx^"VY"^vy^"T"^t)
    done;
    !coords;;

let get_ocoords obs =
    let coords = ref "" in 
    for i=0 to (List.length obs)-1 do 
        let x=string_of_float(List.nth (List.nth obs i) 0) and y=string_of_float(List.nth (List.nth obs i) 1) in
        if i=0 then
            coords := ("X"^x^"Y"^y)
        else
            coords := (!coords^"|X"^x^"Y"^y)
    done;
    !coords;;

(* NEWCOM/A1.57T2/ *)


let get_scores players =
    let scores = ref "" in 
    for i=0 to (List.length players)-1 do 
        let p = List.nth players i in 
        if i=0 then
            scores := (p.username^":"^(string_of_int !(p.score)))
        else 
            scores := (!scores^"|"^p.username^":"^(string_of_int !(p.score)))
    done;
    !scores;;




let stay_in_range x y =
    let newX = ref x and newY = ref y in
    if x>400.0 then 
        newX := -400.0;
    if x<(-400.0) then 
        newX := 400.0;
    
    if y>300.0 then 
        newY := -300.0;
    if y<(-300.0) then 
        newY := 300.0;    
    (!newX,!newY);;
    



    let compute_new_coord_vitesse player = 
        (* Calcul du nouveau vecteur vitesse *)
    
         player.direction := (((List.nth !(player.commande) 0) +. !(player.direction)));
    
        let angle = !(player.direction) and poussee = (List.nth !(player.commande) 1) in
        let push = [ poussee*.(Pervasives.cos(angle)) ; poussee*.(Pervasives.sin(angle))] in 
        let new_vect_vit = ref [ (List.nth push 0)+.(List.nth !(player.vect_vitesse) 0) ; 
                                 (List.nth push 1)+.(List.nth !(player.vect_vitesse) 1)] in
        let h = Pervasives.hypot (List.nth (!new_vect_vit) 0) (List.nth (!new_vect_vit) 1) in 
            (* if h > max_speed then begin
                new_vect_vit := [Pervasives.cos(List.nth (!new_vect_vit) 0) *. max_speed; 
                                 Pervasives.cos(List.nth (!new_vect_vit) 1) *. max_speed];
            end; *)
            player.vect_vitesse := !new_vect_vit;    
            (* player.direction := angle +. !(player.direction);      *)
        (* Calcul de la nouvelle position *)
        let oldX = (List.nth !(player.position) 0) and oldY = (List.nth !(player.position) 1) in 
        let vectX = (List.nth !(player.vect_vitesse) 0) and vectY = (List.nth !(player.vect_vitesse) 1) in  
        let newX = oldX+.vectX and newY = oldY+.vectY in 
        let (x,y) = stay_in_range newX newY in 
            player.position := [ x ; y ];
        
        
        !(player.position);;


let check_goal new_coord index = 
    let obj_atteint = ref false in
    let win = ref false in
    let dist = (Pervasives.hypot ((List.nth !goal 0)-.(List.nth new_coord 0)) ((List.nth !goal 1)-.(List.nth new_coord 1))) in
        if dist <= obj_radius then begin
            obj_atteint := true;
            let p = (List.nth !players index) in 
                p.score := !(p.score) + 1;
                if !(p.score) = win_cap then begin
                    win := true;
                end;
            Random.init (int_of_float (100000.0*.(Sys.time())));
            goal := [(Random.float 800.0)-.400.0; (Random.float 600.0)-.300.0];
        end;

    (* let obs_atteint = ref false in  *)
    for i=0 to (List.length !obstacles)-1 do 
        let o = (List.nth !obstacles i) in 
        let dist_obs = (Pervasives.hypot ((List.nth o 0)-.(List.nth new_coord 0)) ((List.nth o 1)-.(List.nth new_coord 1))) in
            
            if dist_obs <= ob_radius then begin
                print_string "Collision avec obstacle\n";
                flush stdout;
                let p = (List.nth !players index) in 
                    p.vect_vitesse := [-.(List.nth !(p.vect_vitesse) 0);-.(List.nth !(p.vect_vitesse) 1)]  
            end;
    done;

    for i=0 to (List.length !players)-1 do 
        if i!=index then begin
            let v = (List.nth !players i) in 
            let dist_veh = (Pervasives.hypot ((List.nth !(v.position) 0)-.(List.nth new_coord 0)) ((List.nth !(v.position) 1)-.(List.nth new_coord 1))) in
                if dist_veh <= ve_radius then begin
                    print_string "Collision avec vehicule\n";
                    flush stdout;
                    let player = (List.nth !players index) in 
                        player.vect_vitesse := [-.(List.nth !(player.vect_vitesse) 0);-.(List.nth !(player.vect_vitesse) 1)];
                    let oldX = (List.nth !(player.position) 0) and oldY = (List.nth !(player.position) 1) in 
                    let vectX = (List.nth !(player.vect_vitesse) 0) and vectY = (List.nth !(player.vect_vitesse) 1) in  
                        player.position := [ oldX+.vectX ; oldY+.vectY ];  
                end;
        end;
    done;

    (!obj_atteint,!win);;


let refresh players =
    while !reset do
        Thread.delay (!server_refresh_tickrate);
        Mutex.lock mut;
        for i=0 to (List.length !players)-1 do
            (* Calcul de la nouvelle postion  & check goal *)
            let player = (List.nth !players i) in 
                let new_coord = compute_new_coord_vitesse player in
                    let (obj,win) = check_goal new_coord i in
                        if win then begin 
                            let msg = ("WINNER/"^(get_scores !players)^"/\n") in 
                                send_to_all players msg "";
                                Condition.signal cond;
                        end
                        else begin
                            if obj then
                                let msg = ("NEWOBJ/"^(goal_string goal)^"/"^(get_scores !players)^"\n") in 
                                    send_to_all players msg "";
                        end;
                player.commande := [0.0;0.0];
                let msg = ("TICK/"^(get_vcoords !players)^"/\n") in 
                send_to_all players msg ""
        done;
        Mutex.unlock mut
    done;
    Thread.exit ();;





let jeu () = 
    while !reset do 
        Thread.delay (!server_tickrate);
        let msg = ("TICK/"^(get_vcoords !players)^"/\n") in 
            send_to_all players msg ""
    done;
    Thread.exit ();;




let init () =
    for i=0 to (List.length !players)-1 do
        let p = (List.nth !players i) in
            p.score := 0;
            Random.init (int_of_float (100000.0*.(Sys.time())));
            p.position := [(Random.float 800.0)-.400.0; (Random.float 600.0)-.300.0]
    done;
    Random.init (int_of_float (100000.0*.(Sys.time())));
    goal := [(Random.float 800.0)-.400.0; (Random.float 600.0)-.300.0]
    ;;

let split_comm comms = 
    let c = List.nth (String.split_on_char 'A' comms) 1 in
    let c = String.split_on_char 'T' c in
    let c = List.map (function x -> float_of_string x) c in
    c;;

let create_obstacles nb = 
    let l = ref [] in 
        for i=0 to nb-1 do 
            Random.init (int_of_float (100000.0*.(Sys.time())));
            l := (List.append !l [[(Random.float 800.0)-.400.0; (Random.float 600.0)-.300.0]])
        done;
    !l;;
  

let create_sock port nb =
    let sock = ThreadUnix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
    let host = Unix.gethostbyname (Unix.gethostname()) in
    let h_addr = "127.0.0.1" in (*host.Unix.h_addr_list.(0) in*)
    Unix.bind sock (Unix.ADDR_INET((Unix.inet_addr_of_string h_addr),port));
    Unix.listen sock nb;
    sock;;



let listener_service desc =
        let inchan = Unix.in_channel_of_descr desc and 
            outchan = Unix.out_channel_of_descr desc in
        while true do  
            let line = input_line inchan in
            let sp = String.split_on_char '/' line in 
            let first = List.nth sp 0 in 
                match first with
                    | "CONNECT" ->
                        Mutex.lock mut;
                        print_string "Connecté\n";
                        flush stdout;
                        let username = List.nth sp 1 in
                        if not(user_exists players username) then begin
                            if !nb_clients = 0 then begin
                                Random.init (int_of_float (100000.0*.(Sys.time())));
                                obstacles := create_obstacles 15;
                            end;
                            
                            players := (List.append !players [{username=username;
                                (* position=ref [(Random.float 800.0)-.400.0; (Random.float 600.0)-.300.0]; *)
                                (* position=ref (List.nth !obstacles 0); *)
                            position = ref [0.0;2.0];
                            vect_vitesse=ref [0.0;-1.0]; direction = ref 0.0; commande=ref [0.0;0.0];score=ref 0;inchan=inchan;outchan=outchan}]);  
                            
                            nb_clients := !nb_clients + 1; 
                            Condition.signal(first_cnx);
                            output_string outchan ("WELCOME/"^(!phase)^"/"^(get_scores !players)^"/"^(goal_string goal)^"/"^(get_ocoords !obstacles)^"/\n");
                            flush outchan;
                            let msg = ("NEWPLAYER/"^username^"/\n") in 
                                send_to_all players msg username
                        end
                        else begin
                            output_string outchan ("DENIED/\n");
                            flush outchan
                        end;
                        Mutex.unlock mut;
                    | "EXIT" ->
                        Mutex.lock mut;
                        print_string "Exit\n";
                        flush stdout;
                        let index = get_index_by_inchan players inchan in 
                        let player = (List.nth !players index) in
                            nb_clients := !nb_clients-1;
                            players := (delete_user (ref []) players player);
                        let msg = ("PLAYERLEFT/"^(player.username)^"/\n") in 
                            send_to_all players msg (player.username);
                        if (List.length !players)=0 then begin
                            Condition.signal(cond);
                        end;
                        Mutex.unlock mut;
                        
                        close_in inchan; 
                        
                        Thread.exit ();

                    | "NEWCOM" ->
                        Mutex.lock mut;
                        print_string "Nouvelle commande\n";
                        flush stdout;
                        let comms = List.nth sp 1 in 
                        let new_comm = split_comm comms in 
                        let index = get_index_by_inchan players inchan in 
                        let player = (List.nth !players index) in
                        player.commande := [(List.nth !(player.commande) 0)+.(List.nth new_comm 0);
                                        (List.nth !(player.commande) 1)+.(List.nth new_comm 1)];
                        
                        Mutex.unlock mut;
                    | _ ->
                        print_string "Protocole non réspecté\n";
                        flush stdout
                    ;
        done;;

let connexions sock = 
    while true do
        let (desc,client_sock) = Unix.accept sock in
            Thread.create listener_service desc;
            (* close_in inchan; close_out outchan;
            Unix.close desc  *)
    done;;




let main port =
    let sock = create_sock port 1000 in 
        Thread.create connexions sock;
        Condition.wait(first_cnx);
    while true do 
        Thread.delay 10.0;
        if !nb_clients>0 then begin
            (* Session ouverte *)
            reset := true;
            phase := "jeu";
            let msg = ("SESSION/"^(get_coords !players)^"/"^(goal_string goal)^"/"^(get_ocoords !obstacles)^"/\n") in 
                send_to_all players msg "";
            (* Thread.create jeu (); *)
            Thread.create refresh players;
            Condition.wait cond mut1;
            reset := false;
            phase := "attente";
            init ();
            Mutex.unlock mut1;
        end
        else begin
            print_string ("Il n'y a aucun joueur\n");
            flush stdout;
        end;        
    done;;

let _ = main (int_of_string Sys.argv.(1));;
