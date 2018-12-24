//#![feature(trace_macros)]
//trace_macros!(true);

use libccsrs::{Program, Process, parser::Parser};
//use libccsrs::macros::*;

use getopts::Options;

use rand::{Rng, thread_rng};

use std::collections::{HashSet, HashMap, hash_map::Entry};
use std::env;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use std::process;
use std::rc::Rc;
use std::time::Instant;

enum Command {
    Graph, Random, Echo
}

struct Args {
    depth: Option<usize>,
    ignore: bool,
    omit_names: bool,
    trace: bool,
    weak: bool
}

fn main() {
    let mut opts = Options::new();
    opts.optopt("d", "depth", "Limits the depth of LTS exploration", "DEPTH");
    opts.optflag("i", "ignore-errors", "Ignores errors during LTS exploration");
    opts.optflag("", "omit-names", "Does not print the CCS process expressions into the nodes");
    opts.optflag("t", "trace", "Only show traces");
    opts.optflag("T", "weak-trace", "Only show traces, but hide weak steps");
    opts.optflag("h", "help", "Prints this help message");

    let args: Vec<String> = env::args().collect();
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("ccsrs: {}", f);
            eprintln!("Try 'ccsrs --help' for more information.");
            process::exit(1)
        }
    };

    if matches.opt_present("help") {
        eprint!("{}", opts.usage(concat!(
            "Usage: ccsrs [options] <command> [input-file]\n",
            "\n",
            "Commands:\n",
            "    graph\n",
            "        Output a graph of the LTS in DOT format\n",
            "    random\n",
            "        Traverse a random path through the LTS\n",
            //"    actions\n",
            //"        Search for all actions\n",
            //"    dead\n",
            //"        Search for deadlocks (states with no outgoing transitions)\n",
            //"    ttr\n",
            //"        Search for terminating traces\n",
            "    echo\n",
            "        Outputs the CCS program (for debugging)"
        )));
        process::exit(0)
    }

    if matches.free.len() < 1 {
        eprintln!("ccsrs: no command given");
        eprintln!("Try 'ccsrs --help' for more information.");
        process::exit(1)
    }

    let mut args = Args {
        depth: None,
        ignore: false,
        omit_names: false,
        trace: false,
        weak: false
    };
    if let Some(s) = matches.opt_get::<String>("depth").unwrap() {
        args.depth = Some(match s.parse() {
            Ok(d) => d,
            Err(_) => {
                eprintln!("ccsrs: invalid depth: {}", s);
                process::exit(1)
            }
        });
    }
    if matches.opt_present("ignore-errors") {
        args.ignore = true;
    }
    if matches.opt_present("omit-names") {
        args.omit_names = true;
    }
    if matches.opt_present("trace") {
        args.trace = true;
    }
    if matches.opt_present("weak-trace") {
        args.trace = true;
        args.weak = true;
    }

    let cmd = match &matches.free[0][..] {
        "graph" => Command::Graph,
        "random" => Command::Random,
        "echo" => Command::Echo,
        cmd => {
            eprintln!("ccsrs: unknown command: {}", cmd);
            eprintln!("Try 'ccsrs --help' for more information.");
            process::exit(1)
        }
    };

    let program_res = 
        if matches.free.len() >= 2 {
            match File::open(&matches.free[1]) {
                Ok(f) =>
                    Parser::new(matches.free[1].clone(), BufReader::new(f).bytes())
                        .parse_program(),
                Err(e) => {
                    eprintln!("ccsrs: Cannot open file {}: {}", &matches.free[1], e);
                    process::exit(1)
                }
            }
        } else {
            Parser::new("stdin".to_string(), io::stdin().bytes())
                .parse_program()
        };

    let program = match program_res {
        Ok(p) => p,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1)
        }
    };

    match cmd {
        Command::Graph =>
            process::exit(cmd_graph(&args, &program)),
        Command::Random =>
            process::exit(cmd_random(&args, &program)),
        Command::Echo => {
            println!("{}", &program);
        }
    }

}

fn print_node(args: &Args, id: usize, p: &Rc<Process>, error: bool, explored: bool, term: bool) {
    if args.omit_names {
        print!("    p{} [label=\"\"", id);
    } else {
        print!("    p{} [label={:?}", id, format!("{}", p));
    }
    if term {
        print!(",shape=box");
    }
    if !explored {
        print!(",style=dashed");
    }
    if error {
        print!(",color=red");
    }
    println!("];");
}

fn cmd_graph(args: &Args, program: &Program) -> i32 {
    println!("digraph lts {{");
    println!("    start [shape=point];");
    println!("    start -> p0;");

    let mut errors = 0;
    let mut nodes = HashMap::new();
    let mut q = HashSet::new();
    let start = program.process().unwrap();
    nodes.insert(Rc::clone(&start), 0);
    q.insert(start);

    let t_start = Instant::now();
    let mut n_states = 0usize;
    let mut n_trans = 0usize;

    let mut d = 0usize;
    while !q.is_empty() {
        if let Some(max) = args.depth {
            if d >= max {
                break;
            }
            d += 1;
        }

        let q2 = q;
        q = HashSet::new();
        for next in q2 {
            let id = nodes[&next];
            n_states += 1;
            match next.get_transitions(&program) {
                Ok(trans) => {
                    print_node(args, id, &next, false, true, trans.is_empty());
                    n_trans += trans.len();
                    for tr in trans {
                        let id_next = nodes.len();
                        let id2 = match nodes.entry(Rc::clone(&tr.to)) {
                            Entry::Occupied(v) => *v.get(),
                            Entry::Vacant(v) => {
                                q.insert(Rc::clone(&tr.to));
                                *v.insert(id_next)
                            }
                        };
                        println!("    p{} -> p{} [label={:?}];", id, id2, format!("{}", tr.act));
                    }
                },
                Err(err) => {
                    eprintln!("Error: {}", err);
                    eprintln!("On process: {}", next);
                    if args.ignore {
                        print_node(args, id, &next, true, true, false);
                        errors += 1;
                    } else {
                        return 1;
                    }
                }
            }
        }
    }

    let t_diff = t_start.elapsed();
    eprintln!("visited {} states / computed {} transitions in {}.{:03}s", n_states, n_trans, t_diff.as_secs(), t_diff.subsec_millis());

    for next in q {
        let id = nodes[&next];
        print_node(args, id, &next, false, false, false);
    }
    println!("}}");

    if errors > 0 { 1 } else { 0 }
}

fn cmd_random(args: &Args, program: &Program) -> i32 {
    let mut rng = thread_rng();

    let mut p = program.process().unwrap();
    if !args.trace {
        println!("{}", p);
    }

    let t_start = Instant::now();
    let mut n_states = 0usize;
    let mut n_trans = 0usize;

    let mut d = 0usize;
    loop {
        if let Some(max) = args.depth {
            if d >= max {
                break;
            }
            d += 1;
        }

        n_states += 1;
        let trans = match p.get_transitions(program) {
            Ok(trans) => trans,
            Err(err) => {
                eprintln!("Error: {}", err);
                return 1;
            }
        };
        if trans.is_empty() {
            break;
        }
        n_trans += trans.len();

        let next_i = rng.gen_range(0, trans.len());
        let next = trans.into_iter().nth(next_i).unwrap();
        if !args.trace {
            println!("    --( {} )->", next.act);
            println!("{}", next.to);
        } else if !args.weak {
            println!("{}", next.act);
        } else {
            if let Some(_) = next.act.observe() {
                println!("{}", next.act);
            }
        }
        p = next.to;
    }

    let t_diff = t_start.elapsed();
    eprintln!("visited {} states / computed {} transitions in {}.{:03}s", n_states, n_trans, t_diff.as_secs(), t_diff.subsec_millis());

    0
}
