use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;

use super::char_set::CharSet;

use super::pattern::{Pattern, RepetitionKind};

/// Requires that there is only one repetition vertex between nodes
type LoopCounts = HashMap<(NodeId, NodeId), LoopFrame>;

/// Returns length of a match, if any
pub fn match_start(pattern: &Pattern, text: &str) -> Option<usize> {
    let graph = build_graph(pattern);

    let text_chars: Vec<char> = text.chars().collect();

    match_start_rec(&graph, &text_chars, NodeId(0), 0, LoopCounts::new())
}

/// Returns (index, length) of a match, if any, first matcv only
pub fn match_find(pattern: &Pattern, text: &str) -> Option<(usize, usize)> {
    let graph = build_graph(pattern);

    let text_chars: Vec<char> = text.chars().collect();

    for i in 0..text_chars.len() {
        if let Some(l) = match_start_rec(&graph, &text_chars[i..], NodeId(0), 0, LoopCounts::new())
        {
            return Some((i, l));
        }
    }

    None
}

macro_rules! return_if_some {
    ($opt:expr) => {
        match $opt {
            Some(v) => return Some(v),
            None => {}
        }
    };
}

fn match_start_rec(
    graph: &Graph,
    text: &[char],
    current: NodeId,
    text_index: usize,
    loop_counts: LoopCounts,
) -> Option<usize> {
    let vertices = graph.vertices_from(current);

    if text.is_empty() {
        if graph.accepts_empty(current) {
            return Some(0);
        } else {
            return None;
        }
    }

    if vertices.is_empty() {
        if graph.accept == Some(current) {
            return Some(text_index);
        } else {
            return None;
        }
    }

    for v in vertices.iter().rev() {
        match v.condition {
            VertexCondition::None | VertexCondition::PreferNone => {
                return_if_some!(match_start_rec(
                    graph,
                    text,
                    v.end,
                    text_index,
                    loop_counts.clone()
                ));
            }
            VertexCondition::CharSet(ref char_set) => {
                if char_set.accepts(text[text_index]) {
                    if text.len() == text_index + 1 {
                        if Some(v.end) == graph.accept {
                            return Some(text_index + 1);
                        }
                    } else {
                        return_if_some!(match_start_rec(
                            graph,
                            text,
                            v.end,
                            text_index + 1,
                            loop_counts.clone()
                        ));
                    }
                }
            }
            VertexCondition::Repetition(kind) => {
                let key = (v.start, v.end);
                let frame = loop_counts.get(&key).unwrap_or(&LoopFrame::new()).clone();

                let new_frame = LoopFrame {
                    count: frame.count + 1,
                    text_index: text_index,
                    success: frame.success.or(if Some(v.start) == graph.accept {
                        Some(text_index)
                    } else {
                        None
                    }),
                };

                let mut lc = loop_counts.clone();
                lc.insert(key, new_frame);

                if frame.count < kind.min {
                    // Not enough repetitions, discard new_items and loop back
                    return match_start_rec(graph, text, v.end, text_index, lc);
                }

                if kind.max.map(|max| frame.count < max).unwrap_or(true) {
                    // Looping back without consuming more text is not allowed
                    if frame.text_index != text_index {
                        return_if_some!(match_start_rec(
                            graph,
                            text,
                            v.end,
                            text_index,
                            lc.clone()
                        ));
                    }
                }
            }
        };
    }

    if Some(current) == graph.accept {
        Some(text_index)
    } else {
        None
    }
}

#[derive(Debug, Clone)]
struct LoopFrame {
    count: usize,
    text_index: usize,
    /// Success has been reached before, and will be used as a fallback if looping more fails
    success: Option<usize>,
}
impl LoopFrame {
    fn new() -> Self {
        Self {
            count: 0,
            text_index: 0,
            success: None,
        }
    }
}

pub fn build_graph(pattern: &Pattern) -> Graph {
    let mut graph = Graph::new();

    let start_id = graph.add_node();
    let accept_state = graph_add(pattern, &mut graph, start_id);
    graph.accept = Some(accept_state);
    graph.simplify();
    graph
}

/// Returns the end point after this phase
fn graph_add(pattern: &Pattern, graph: &mut Graph, start: NodeId) -> NodeId {
    match pattern {
        Pattern::Concat(items) => {
            let mut prev = start;
            for item in items {
                prev = graph_add(item, graph, prev);
            }
            prev
        }
        Pattern::Alternation(items) => {
            let colllector = graph.add_node();
            for item in items {
                let end = graph_add(item, graph, start);
                graph.add_vertex(Vertex::new(end, colllector, VertexCondition::None));
            }

            colllector
        }
        Pattern::Repetition { pattern, kind } => {
            let rep_start = graph.add_node();
            graph.add_vertex(Vertex::new(start, rep_start, VertexCondition::None));

            let end = graph_add(&pattern, graph, rep_start);

            if kind.min == 0 {
                graph.add_vertex(Vertex::new(
                    rep_start,
                    end,
                    if kind.greedy {
                        VertexCondition::None
                    } else {
                        VertexCondition::PreferNone
                    },
                ));
            }

            if kind.max != Some(1) {
                let condition = VertexCondition::Repetition(*kind);
                graph.add_vertex(Vertex::new(end, rep_start, condition));
            }

            end
        }
        Pattern::CharSet(cs) => {
            let new_node = graph.add_node();
            graph.add_vertex(Vertex::new(
                start,
                new_node,
                VertexCondition::CharSet(cs.clone()),
            ));
            new_node
        }
    }
}

/// Sort first by start, then by condition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vertex {
    start: NodeId,
    condition: VertexCondition,
    end: NodeId,
}
impl Vertex {
    fn new(start: NodeId, end: NodeId, condition: VertexCondition) -> Self {
        Vertex {
            start,
            end,
            condition,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum VertexCondition {
    None,
    CharSet(CharSet),
    Repetition(RepetitionKind),
    /// True on non-greedy matches, where None-branch is preferred
    PreferNone,
}
impl fmt::Display for VertexCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "()"),
            Self::CharSet(set) => write!(f, "{}", set),
            Self::Repetition(kind) => write!(f, "rep {}", kind),
            Self::PreferNone => write!(f, "(prefer)"),
        }
    }
}
impl Ord for VertexCondition {
    /// Sort order:
    /// * Non-greedy repetitions
    /// * Non-preferred Nones
    /// * CharSets
    /// * Greedy repetitions (totally sorted)
    /// * Preferred Nones
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            Self::Repetition(a) if !a.greedy => match other {
                Self::Repetition(b) if !b.greedy => a.cmp(b),
                _ => Ordering::Less,
            },
            Self::None => match other {
                Self::Repetition(a) if !a.greedy => Ordering::Greater,
                Self::None => Ordering::Equal,
                _ => Ordering::Less,
            },
            Self::CharSet(_) => match other {
                Self::Repetition(r) if !r.greedy => Ordering::Greater,
                Self::None { .. } => Ordering::Greater,
                Self::CharSet(_) => Ordering::Equal,
                Self::Repetition(_) => Ordering::Less,
                Self::PreferNone { .. } => Ordering::Less,
            },
            Self::Repetition(a) => match other {
                Self::Repetition(b) if !b.greedy => Ordering::Greater,
                Self::Repetition(b) => a.cmp(b),
                Self::PreferNone { .. } => Ordering::Less,
                _ => Ordering::Greater,
            },
            Self::PreferNone => match other {
                Self::PreferNone => Ordering::Equal,
                _ => Ordering::Greater,
            },
        }
    }
}

impl PartialOrd for VertexCondition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(usize);
impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Graph {
    next_node: NodeId,
    vertices: Vec<Vertex>,
    accept: Option<NodeId>,
}
impl Graph {
    fn new() -> Self {
        Self {
            next_node: NodeId(0),
            vertices: Vec::new(),
            accept: None,
        }
    }

    fn add_node(&mut self) -> NodeId {
        let id = self.next_node;
        self.next_node.0 += 1;
        id
    }

    fn add_vertex(&mut self, vertex: Vertex) {
        match self.vertices.binary_search(&vertex) {
            Ok(_) => {} // vertex already exists, do not insert duplicate
            Err(pos) => self.vertices.insert(pos, vertex),
        }
    }

    /// Vertices starting from a point
    pub fn vertices_from(&self, node: NodeId) -> &[Vertex] {
        let mut i = 0;
        while i < self.vertices.len() {
            if self.vertices[i].start == node {
                let start = i;
                while i < self.vertices.len() && self.vertices[i].start == node {
                    i += 1;
                }
                return &self.vertices[start..i];
            }
            i += 1;
        }

        // Empty slice
        &self.vertices[0..0]
    }

    /// Is empty string accepted from this position
    pub fn accepts_empty(&self, node: NodeId) -> bool {
        if Some(node) == self.accept {
            return true;
        }

        for v in self.vertices_from(node) {
            if v.condition == VertexCondition::None || v.condition == VertexCondition::PreferNone {
                if self.accepts_empty(v.end) {
                    return true;
                }
            }
        }

        false
    }

    /// Simplify the graph:
    /// * Remove unnecessary empty transitions
    /// TODO:
    /// * Merge char sets together
    /// * Combine repetition operators
    fn simplify(&mut self) {
        self.simplify_remove_empty();
    }

    /// Remove unnecessary empty transitions
    fn simplify_remove_empty(&mut self) {
        let mut remove_vertices = Vec::new();
        for vertex in &self.vertices {
            if !matches!(vertex.condition, VertexCondition::None {..}) {
                continue;
            }

            let start_out = self
                .vertices
                .iter()
                .filter(|v| v.start == vertex.start)
                .count();

            if start_out == 1 {
                remove_vertices.push(vertex.clone());
            }
        }
        for vertex in remove_vertices {
            self.merge_nodes(&[vertex.start, vertex.end]);
        }
    }

    /// Merge a set of nodes into one node
    fn merge_nodes(&mut self, nodes: &[NodeId]) -> NodeId {
        assert!(!nodes.is_empty());

        let target = *nodes.iter().min().unwrap();

        for node in nodes {
            if *node == target {
                continue;
            }

            for vertex in &mut self.vertices {
                if nodes.contains(&vertex.end) {
                    vertex.end = target;
                }

                if nodes.contains(&vertex.start) {
                    vertex.start = target;
                }
            }
        }

        self.vertices.drain_filter(|v| {
            v.start == v.end && matches!(v.condition, VertexCondition::None {..})
        });

        self.vertices.sort();
        self.vertices.dedup();

        if let Some(accept) = self.accept {
            if nodes.contains(&accept) {
                self.accept = Some(target);
            }
        }

        target
    }

    /// Formats graph to graphwiz dot format
    pub fn to_gw_dot(&self) -> String {
        let mut result = String::new();

        result.push_str("digraph A {\n");
        result.push_str("  rankdir=LR;\n");
        result.push_str("  node [shape=circle,label=\"\"]\n");
        result.push_str("  start [style=invis]\n");
        result.push_str("  start -> n0\n");

        for vertex in &self.vertices {
            result.push_str(&format!(
                "  {} -> {} [label=\"{}\"] \n",
                vertex.start, vertex.end, vertex.condition
            ));
        }

        if let Some(accept) = self.accept {
            result.push_str(&format!("  {} [shape=doublecircle]\n", accept))
        }

        result.push_str("}\n");

        result
    }
}

#[cfg(test)]
mod test_matcher {
    use super::match_start;
    use crate::pattern::parse;

    #[test]
    fn test_match_start_greedy() -> Result<(), String> {
        assert_eq!(
            match_start(&parse(r"<.+>")?, "<tag>Greedy?</tag>"),
            Some(18)
        );

        assert_eq!(
            match_start(&parse(r"<.+?>")?, "<tag>Greedy?</tag>"),
            Some(5)
        );

        assert_eq!(
            match_start(&parse(r"<tag>?")?, "<tag>Greedy?</tag>"),
            Some(5)
        );

        assert_eq!(
            match_start(&parse(r"<tag>??")?, "<tag>Greedy?</tag>"),
            Some(4)
        );

        Ok(())
    }

    macro_rules! matches_all {
        ($p:expr, $s:literal) => {
            match_start(&$p, $s) == Some($s.len())
        };
    }

    #[test]
    fn test_match_start_simple0() -> Result<(), String> {
        let p = parse(r"(a|bb)+")?;
        assert!(!matches_all!(p, ""));
        assert!(matches_all!(p, "a"));
        assert!(matches_all!(p, "bb"));
        assert!(matches_all!(p, "abb"));
        assert!(matches_all!(p, "bba"));
        assert!(!matches_all!(p, "bbb"));
        assert!(!matches_all!(p, "bab"));
        assert!(!matches_all!(&p, "bbb"));
        assert!(!matches_all!(&p, "bab"));

        Ok(())
    }

    #[test]
    fn test_match_start_simple1() -> Result<(), String> {
        let p = parse(r"(a|bb)+")?;
        assert!(matches_all!(&p, "a"));
        assert!(matches_all!(&p, "bb"));
        assert!(matches_all!(&p, "abb"));
        assert!(matches_all!(&p, "bba"));
        assert!(!matches_all!(&p, "bbb"));
        assert!(!matches_all!(&p, "bab"));

        Ok(())
    }

    #[test]
    fn test_match_start_odd_number() -> Result<(), String> {
        let p = parse(r"-?([1-9][0-9]*)?[13579]")?;
        assert!(matches_all!(&p, "1"));
        assert!(matches_all!(&p, "-51"));
        assert!(matches_all!(&p, "19"));
        assert!(!matches_all!(&p, "test"));
        assert!(!matches_all!(&p, "0"));
        assert!(!matches_all!(&p, "2"));
        assert!(!matches_all!(&p, "12"));
        assert!(!matches_all!(&p, "-20"));

        Ok(())
    }
}
