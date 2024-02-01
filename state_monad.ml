open Game_defs
open GameState

(** Monada stanu -- obliczenia z ukrytą komórką mutowalnego stanu *)
module StGameState : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val get  : GameState.t t
  val getA : PlayerState.t t
  val getB : PlayerState.t t

  val set  : GameState.t   -> unit t
  val setA : PlayerState.t -> unit t
  val setB : PlayerState.t -> unit t

  val run  : GameState.t   -> 'a t -> 'a
  
end = struct
  (* Obliczenie reprezentujemy jako funkcję z bieżącej wartości stanu w parę
   * wynik-nowy stan *)
  type 'a t = GameState.t -> 'a * GameState.t

  let return x s = (x, s)
  let bind m f s =
    let (x, s) = m s in
    f x s

  let get  s = (s, s)
  let getA s = (s.playerA, s)
  let getB s = (s.playerB, s)

  let set s _ = ((), s)
  let setA player_state s = ((), { s with GameState.playerA = player_state })
  let setB player_state s = ((), { s with GameState.playerB = player_state })

  let run s m = fst (m s)
end

let (>>=) = StGameState.bind