(* 定义网页和计算任务的类型 *)
type webpage = {
  start_time: int;     (* 网页开始的时间 *)
  deadline: int;       (* 网页的截止时间 *)
  homework_needed: int;(* 需要完成的作业数 *)
}

type computation_task = {
  duration: int;       (* 计算任务的持续时间 *)
}

(* 读取输入数据 *)
let read_input () =
  (* 读取 P 和 H *)
  let p, h = Scanf.scanf "%d %d\n" (fun p h -> p, h) in
  (* 读取网页信息 *)
  let webpages =
    let rec read_webpages n acc =
      if n = 0 then acc
      else
        let si, ti, hi =
          Scanf.scanf "%d %d %d\n" (fun si ti hi -> si, ti, hi)
        in
        read_webpages (n - 1) ({start_time = si; deadline = ti; homework_needed = hi} :: acc)
    in
    read_webpages p []
  in
  (* 读取计算任务数量 T *)
  let t = Scanf.scanf "%d\n" (fun t -> t) in
  (* 读取计算任务信息 *)
  let computation_tasks =
    let rec read_tasks n acc =
      if n = 0 then acc
      else
        let duration = Scanf.scanf "%d\n" (fun duration -> duration) in
        read_tasks (n - 1) ({duration} :: acc)
    in
    read_tasks t []
  in
  (webpages, computation_tasks, h)

(* 计算并打印每个计算任务的完成时间 *)
let schedule_computations computation_tasks =
Printf.printf "Task finish time:\n";
  let current_time = ref 0 in
  List.iter (fun task ->
    current_time := !current_time + task.duration;
    Printf.printf "%d\n" !current_time;
  ) computation_tasks

(* 处理网页调度和朋友的作业分配 *)
let schedule_webpages webpages h =
  (* 这里将网页按截止时间升序排序 *)
  let sorted_webpages = List.sort (fun a b -> compare a.deadline b.deadline) webpages in
  let current_time = ref 0 in
  (* 对每个网页进行调度 *)
  List.iter (fun webpage ->
    (* 每个网页任务从当前时间开始，当前时间加上网页的持续时间 *)
    current_time := !current_time + webpage.start_time;
    (* 打印每个网页的完成时间 *)
    (*Printf.printf "%d\n" !current_time;*)
  ) sorted_webpages

(* 主入口 *)
let () =
  let webpages, computation_tasks, h = read_input () in
  (* 调度网页任务并输出网页完成时间 *)
  schedule_webpages webpages h;
  (* 调度计算任务并输出任务完成时间 *)
  schedule_computations computation_tasks
