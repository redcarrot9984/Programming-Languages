(* 頂点は文字列、辺は (始点, 終点, 重み) のタプルで表現 *)
type vertex = string
type edge = vertex * vertex * int
type graph = edge list

val sampleGraph : graph = [
  ("A", "B", 2),
  ("A", "C", 3),
  ("B", "C", 1),
  ("C", "D", 4),
  ("D", "A", 5)
]

(* 頂点の次数を数える関数 *)
fun count_degrees (edges : graph) : (vertex * int) list =
  let
    (* 辺を走査して、各頂点の出現回数を数える補助関数 *)
  
fun add_vertex (v, []) = [(v, 1)]
 | add_vertex (v, (x, n)::rest) =
 if x = v then
 (x, n+1) :: rest
else
(x, n) :: add_vertex (v, rest)

 fun process_edge ((u, v, _), counts) =
 add_vertex (v, add_vertex (u, counts))

  
    (* 各辺の両端の頂点を処理 *)
    fun process_edge ((u, v, _), counts) =
      add_vertex (v, add_vertex (u, counts))

  in
    foldl process_edge [] edges
  end



(* 奇数次数の頂点を抽出する関数（修正版） *)
fun odd_degree_vertices (degrees : (vertex * int) list) : vertex list =
  List.map #1 (List.filter (fn (_, deg) => deg mod 2 = 1) degrees)

val degrees = count_degrees sampleGraph;
val oddVertices = odd_degree_vertices degrees;
(* 出力例: ["C", "A"] *)




(* 隣接する頂点を取得 *)
fun neighbors (g : graph) (v : vertex) : (vertex * int) list =
  List.foldl (fn ((u, w, cost), acc) =>
    if u = v then (w, cost)::acc
    else if w = v then (u, cost)::acc
    else acc
  ) [] g

(* 最短距離を求める（簡易ダイクストラ） *)
fun shortest_path (g : graph) (start : vertex) : (vertex * int) list =
  let
    fun loop ([], dist) = dist
      | loop ((v, d)::rest, dist) =
          let
            val already = List.exists (fn (x, _) => x = v) dist
            val dist' = if already then dist else (v, d)::dist
            val nbrs = neighbors g v
            val new_nodes = List.filter (fn (n, _) => not (List.exists (fn (x, _) => x = n) dist')) nbrs
            val updated = List.map (fn (n, cost) => (n, d + cost)) new_nodes
          in
            loop (rest @ updated, dist')
          end
  in
    loop ([(start, 0)], [])
  end


val sp = shortest_path sampleGraph "A";
(* 出力例: [("A",0),("B",2),("C",3),("D",7)] *)


(* 頂点リストからすべてのペアを作成 *)
fun all_pairs [] = []
  | all_pairs (x::xs) = List.map (fn y => (x, y)) xs @ all_pairs xs

(* 奇数次数の頂点ペアとその距離を列挙 *)
fun pair_distances (g : graph) (vs : vertex list) : (vertex * vertex * int) list =
  let
    val pairs = all_pairs vs
    fun get_distance (u, v) =
      let
        val sp = shortest_path g u
      in
        case List.find (fn (x, _) => x = v) sp of
            SOME (_, d) => (u, v, d)
          | NONE => (u, v, 999999)  (* 到達不能な場合は大きな値を入れる *)
      end
  in
    List.map get_distance pairs
  end


val oddVs = odd_degree_vertices (count_degrees sampleGraph);
val distances = pair_distances sampleGraph oddVs;
(* 出力例: [("A","C",3)] など *)

(* 頂点リストからすべての完全マッチングを列挙 *)
fun matchings [] = [[]]
  | matchings (x::xs) =
      List.concat (
        List.map (fn y =>
          let
            val rest = List.filter (fn z => z <> y) xs
            val submatch = matchings rest
          in
            List.map (fn m => (x, y)::m) submatch
          end
        ) xs
      )


val test = matchings ["A", "B", "C", "D"];
(* 出力例: [[("A","B"),("C","D")], [("A","C"),("B","D")], [("A","D"),("B","C")]] *)


(* 距離リストから2頂点間の距離を取得 *)
fun get_distance dist_list u v =
  case List.find (fn (a, b, _) => (a = u andalso b = v) orelse (a = v andalso b = u)) dist_list of
      SOME (_, _, d) => d
    | NONE => 999999  (* 到達不能な場合は大きな値を返す *)

(* マッチングの合計距離を計算 *)
fun total_distance dist_list matching =
  List.foldl (fn ((u, v), acc) => acc + get_distance dist_list u v) 0 matching


val dists = pair_distances sampleGraph ["A", "C"];
val match = [("A", "C")];
val cost = total_distance dists match;
(* 出力例: 3 *)


(* 最小のマッチングを選ぶ関数 *)
fun min_matching dist_list vs =
  let
    val all = matchings vs
    fun compare (m1, m2) =
      Int.compare (total_distance dist_list m1, total_distance dist_list m2)
  in
    List.foldl (fn (m, best) =>
      if compare (m, best) = LESS then m else best
    ) (hd all) (tl all)
  end


val oddVs = odd_degree_vertices (count_degrees sampleGraph);
val dists = pair_distances sampleGraph oddVs;
val best = min_matching dists oddVs;
(* 出力例: [("A","C")] など、最小距離のペア *)

(* 無向グラフの辺を1本だけ削除する（最初に一致した1本） *)
fun remove_edge_once (e : edge) (g : graph) : graph =
  let
    fun same_edge (u1, v1, w1) (u2, v2, w2) =
      w1 = w2 andalso ((u1 = u2 andalso v1 = v2) orelse (u1 = v2 andalso v1 = u2))

    fun remove_first [] = []
      | remove_first (x::xs) =
          if same_edge e x then xs else x :: remove_first xs
  in
    remove_first g
  end


(* オイラー路を構築する *)
fun find_eulerian_path g start =
  let
    fun neighbors g v =
      List.foldl (fn ((u, w, cost), acc) =>
        if u = v then (w, cost)::acc
        else if w = v then (u, cost)::acc
        else acc
      ) [] g

    fun remove_edge_once (e : edge) (g : graph) : graph =
      let
        fun same_edge (u1, v1, w1) (u2, v2, w2) =
          w1 = w2 andalso ((u1 = u2 andalso v1 = v2) orelse (u1 = v2 andalso v1 = u2))

        fun remove_first [] = []
          | remove_first (x::xs) =
              if same_edge e x then xs else x :: remove_first xs
      in
        remove_first g
      end

    fun build_path ([], path, _) = path
      | build_path (v::stack, path, g) =
          case neighbors g v of
              [] => build_path (stack, v::path, g)
            | (u, w)::rest =>
                let
                  val g' = remove_edge_once (v, u, w) g
                in
                  build_path (u::v::stack, path, g')
                end
  in
    build_path ([start], [], g)
  end


val extendedGraph = sampleGraph @ [("A", "C", 3)];  (* マッチングで追加された辺を含める *)
val path = find_eulerian_path extendedGraph "A";
(* 出力例: ["A", "B", "C", "A", "D", "C"] など *)

fun show_path (path : vertex list) : string =
  String.concatWith " → " path

(* 辺の重みを取得（無向グラフ） *)
fun edge_weight g u v =
  case List.find (fn (a, b, _) => (a = u andalso b = v) orelse (a = v andalso b = u)) g of
      SOME (_, _, w) => w
    | NONE => 0  (* 通らない辺は0とする *)

(* 経路に沿って重みを合計 *)
fun total_weight g (path : vertex list) : int =
  case path of
      [] => 0
    | [_] => 0
    | x::y::rest => edge_weight g x y + total_weight g (y::rest)

val path = find_eulerian_path extendedGraph "D";
val pathStr = show_path path;
val total = total_weight extendedGraph path;

(* 表示 *)
val _ = print ("経路: " ^ pathStr ^ "\n");
val _ = print ("合計距離: " ^ Int.toString total ^ "\n");

