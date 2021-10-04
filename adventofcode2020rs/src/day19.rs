use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

const RULES: &str = "66: 69 116 | 9 115
91: 95 9 | 109 69
14: 110 69 | 15 9
4: 119 9 | 61 69
17: 9 23 | 69 93
37: 118 69 | 94 9
68: 9 80 | 69 19
117: 37 9 | 45 69
132: 9 109
74: 9 25 | 69 126
102: 122 9 | 6 69
98: 89 9 | 99 69
113: 83 9 | 7 69
92: 9 16 | 69 50
33: 9 39 | 69 133
134: 95 69 | 7 9
57: 9 7 | 69 110
31: 9 66 | 69 51
47: 9 3 | 69 83
21: 69 123 | 9 129
104: 9 86 | 69 7
40: 69 24 | 9 34
32: 9 44 | 69 88
45: 49 69 | 48 9
2: 69 88 | 9 23
5: 3 9 | 110 69
108: 20 86
76: 97 9 | 113 69
56: 9 121 | 69 58
29: 69 13 | 9 82
123: 86 20
46: 23 69 | 95 9
19: 7 69 | 109 9
12: 93 9 | 95 69
75: 85 9 | 128 69
127: 93 69 | 83 9
61: 9 7 | 69 23
78: 9 132 | 69 107
121: 88 69 | 23 9
60: 33 69 | 76 9
73: 26 69 | 100 9
116: 9 60 | 69 75
93: 69 9
38: 9 106 | 69 110
107: 9 7 | 69 86
82: 5 69 | 64 9
58: 44 69 | 23 9
34: 69 57 | 9 104
124: 69 106 | 9 44
109: 69 9 | 9 9
48: 69 44 | 9 83
28: 9 83 | 69 7
64: 9 93 | 69 7
54: 9 102 | 69 40
94: 83 69 | 7 9
80: 9 86 | 69 3
62: 86 69 | 83 9
42: 125 69 | 70 9
71: 69 112 | 9 114
8: 42
131: 69 44 | 9 7
88: 69 9 | 9 69
87: 15 69 | 7 9
3: 9 9 | 69 69
9: \"b\"
41: 21 69 | 78 9
65: 9 69 | 69 20
128: 134 69 | 101 9
44: 9 69 | 69 69
51: 9 18 | 69 54
55: 69 79 | 9 5
95: 69 69 | 69 9
110: 9 69
22: 92 69 | 10 9
67: 9 44 | 69 23
7: 9 20 | 69 69
90: 69 96 | 9 68
53: 111 9 | 27 69
125: 69 72 | 9 22
11: 42 31
23: 9 69 | 9 9
114: 69 105 | 9 131
81: 53 69 | 29 9
69: \"a\"
1: 86 9 | 95 69
18: 9 73 | 69 90
83: 20 20
130: 89 69 | 47 9
89: 44 9 | 93 69
15: 9 9
0: 8 11
105: 69 110
13: 135 9 | 14 69
6: 69 105 | 9 132
103: 120 69 | 71 9
85: 1 69 | 2 9
96: 89 9 | 12 69
101: 69 95 | 9 23
39: 69 15 | 9 109
133: 88 69 | 65 9
86: 69 20 | 9 9
122: 127 9 | 43 69
20: 69 | 9
52: 69 109 | 9 3
119: 15 69 | 88 9
77: 9 7 | 69 93
50: 87 9 | 61 69
129: 9 65 | 69 110
97: 69 95 | 9 93
111: 59 69 | 35 9
115: 69 84 | 9 41
84: 36 69 | 4 9
72: 117 9 | 74 69
135: 110 69
112: 62 69 | 124 9
43: 69 3 | 9 15
118: 109 9 | 93 69
49: 69 23 | 9 83
26: 69 67
63: 106 9 | 44 69
70: 103 69 | 81 9
25: 17 9 | 77 69
36: 91 9 | 32 69
10: 69 98 | 9 56
30: 9 109 | 69 3
126: 9 28 | 69 63
16: 9 38 | 69 30
99: 106 9 | 109 69
59: 23 9 | 3 69
120: 130 9 | 55 69
27: 69 91 | 9 108
24: 46 69 | 52 9
35: 9 93 | 69 109
100: 9 101 | 69 80
106: 69 69
79: 44 69 | 65 9";

const MESSAGES: &str = "bbabbbaaaaaabbabbaabaaabbaababbbabbbabbbababbbbbbbbabbbbbbabaaaa
aabbbabbabaaaababbbaaabb
baababbbaababaaaabbaababbabbbaaabaaaabbbaaaabbaaabbaabba
abbbbaaabbaaaababbbabaaabbbabbbbbabaabab
aaabbbbbabbbbbabbabbaabbaaaaababababbbbaaaaabbbabbbbabbababbaaabaaaababa
bbbabaaaabbaaabbbababababababaaaabaabbbabaaaabbbabbabbabbbaaabbaaabaaaaa
bbaaaabbaabbbaabbabaaaba
abaabbabbaaaaabbbbbabaabbabbbababaaababb
baaaaabbbabbbbbbabababba
baaabbbaabbaabababbbaaab
baaaabaababbbbbababbbbaaabbabbaabbababababababbabbbabaab
baabbaaaaaaaaaababbaaabaaabbabaaabbababaaaabaaaaabbaabaaababaabbbaabbbaaaabbabbbabbbbbbb
ababbbaaabaabaababababbabbbaabbaaaabbbbbababbabaabbbbbaa
abaabbaabbbbbbbaaaabbbbaaaababba
ababaaabbaabbaaabbbaaaba
bababbbbbaabbabbabbabaaaabaababababababababaabab
bbaaaaababbbaababbbbbbaa
abaabababababbbaababaaaa
aabbababaaaababababbbbba
aabaababbbaaababbabbabab
baababababbbabbbbbbbbaba
bbbbaaaabbababaabbbabbabababbbab
baaabbbbbbabbbbbaaaabaaa
ababbbbabbaabbaaaababbaaaababaaabbbbaaabababbabbaababbbb
bbbbaababababbbbbabbaabbbabaaaba
abbbabbababbbaaaababbbbb
abbbbbabbabbabbbbaaabbabbaaaabbbbbaabaaabaabbaba
aabbabbabbbbbbabbbabbaaa
bbbbaaaabaababbbbabbbbab
bbababaaabbaaabbabaaaabb
bbbbbbbabaabababbbbababbbabbbaaaababaaaa
abbbbbaabbabbbaabbbababbababbaabbaaabbab
aaabaabbbbabababaaaabababababaabbbabbabb
abbabbabbbaaaabbabaaaababbbaabbabaaabaaa
babbbabbbbbabaaababaaabb
bbaaaaabaabbaaaabbbabbbb
abaaabbbbaababbaaabaaaab
bababaababbaaabbaaaabaab
bbaabaabbabaaaaabbbbbbbaaabaaabaaaabbbaabbaabbbabbaaaabaaabbabbbbbaaaaaabaabaaaaabbaaaba
abbabababbbbababbbaabaab
ababababaabbaabbbbabbaaa
bbbbbaabbbbabaaabaaaabba
aababaaabababbbabbababababaabbbaabbaaabababbaabbbaabbbaa
aabbaabaabbbbabbaabababb
aabaaaaaabbbbbbbabababbbbabaaaabbbaabbba
bbaaaaabaaabaabbaabaaaab
aaaaaaaaabbaaaaabbbbaaababaaabab
baababbaaaaaababbbbaaabb
abbbbaabaabbaabbbaaabbaaabbbbabbabbbaaaabaaaaaaaaabaabba
baabaabaaabbbabbbbaaaabaabaaaaabaaabaabbbabaabbb
abaaaaabbabbbabbbababbaaaabaaabbaabbababbabbbabababaabaabaabbbaa
aaaaaaabababbbbaaabaaabaababababababbabb
abbbabaabbabaababaaaabaa
abbbbbaabababbbbbabaaaba
aabbaabbbaabaaaabbaaabba
bbabababaaaaabaabbbbaabaaaaababb
aabbaabbbbbbbaabaaabaaba
bbbbbaabbabbbbbbabbbaaab
bbababbaabbbbabbbaabbbab
baaabbbbbababbaaaaabbbab
bbaabbbbbbbbbababaaaabbbbaaaababbbaababa
aabaabababbbbaabaaaaabba
abaabbbbaabbbaaabbbabbba
baabaaaaabbbaabaaabababb
baaabababbbababbaaaaaabb
babaababbaaaababbaabbaaaaaaabababbbbbbaabbaaabbbaaaabbabaaaaaabbaabbaaaaaabbbaaaaaabaaba
aaabaabbaaaababaaaaaaaabaabbbbbb
abbbababababbbbabaaaabab
abaabaabbbabababaaabbaaa
bbbbababaabaaaaaaabbbabbaababaabababaaabbbbabbab
bbbbaaabaabbbabbabbbabbabaababbbbababaaa
bbaabbbbaaabbabbaaababaaaabaaababbaabbba
aabaabbabbabbbaaaaaaabbb
aabbabaababbbabbaabbbbab
baababbbbbbbbbabbabbaaaababaabab
babbaababbaabbaaaaaaaaba
aaabbbbabbaaaabbaaabbaaa
aaaabbbbbaaababaababbbbabababbababaabbab
baaaaabbbaaabaababbaaabbabaabbaabbbbaaaababbaaaa
babbabaabbbbaaaababbbbaabbbbabbb
abbbbabaaabababababaabab
bbaaaaabababaababababbab
ababbaabaabbaaaabaabbaba
bbaabbaaabaababaaaabababbbbbaaba
babbbbaaaabbabbaaabbbbbb
baaabbaabababbaaabababaa
aaaabbbabaabababbabbaababbbbabbaaaabaabbaaaabababbbaabbb
bbbbbbabbaaabbaaabbbbaabbbabaababbbbbaaabbabaaabbabbbbba
baabaabaaabaabbaaabbababbabbabaabbabbaabbaaabaab
babaaaaaabbbbaabbbbbbaba
bbbaababaabbbabbaaababba
bababbbbbaaabbbbabbbabbaababbaababbbaaabbbbaaaba
abbbababbbbbaaabbabbaaab
bababaababaabbbabaaabbbbbaabababbbbaabaa
bbbaabbbaabbbbbaabaaabaaaaabbbbbaababbbbbbababbabbabababaaaababaaaabaabb
baaaaabbabaabaaaabbbbaaaaababababbbbabbb
bbbbaabbbbaaabbbbaabbbaabaaababbbbaababb
abaaaabaaabbababbbaabbba
bbabaabbbbaabbaaababbaabaaababaa
aababbbaabaabbbaaababbaabaaabaaaaabbbbba
babbaabbbbbbaabaaabbabbaaabbabaabbaabbab
aababaabbbbababbaabbbaababbabbabbaaaabaaabbaabaabbbaaabb
abaabbbbabaabbaababaaaba
aabbbaabbaaabababbaaabba
baabaaaaaaaabababaaaabab
bbaaaabbbaaabbbaabababba
bbaaaaabaabaaabbaaaaabbb
baaababaabbaaaaababbbbbbabbbbbaabbbbbbabababbbaa
abbbbabaababaabaaaaabaab
baaababaabbbabbbbaabbaab
abaabbaabababaababbaaaaaababaabb
abaababaaabbabbaabaaabba
bbaababbaabaaabbbababaabbbbabbab
babbaaaaabbabbbaabababbb
aabbaaabbabbbbaaaabbbbba
bababaaababaaababbbabbba
bababbbaabbaabababbbabbbbbababababbbbbaabaaababb
abbabbbababbbbaaaabbaabbaabbaaaaabbabbaa
aaaabababbabbbaaaabaabaaabbabbabbbbbaaaabbaaaababaabbabbbbbbbbbababbabaaaabbbaab
bbbaaaaabbaabbbaaabaabbaabaaababbabbbbbabbbabaab
bbaaaababbbaababbabbbaab
aabaaabaaaabaabbbbabbaaababbababbabaababbbbaabba
aaaabababaababbaaabaaabaababaababbaaabbbbabbbaab
bbaabbaaaaaaabababbabbbabaaaabbb
ababbababbabaababbbabbbb
bbbbbbabbbaabbbbbbbabbbbbbbbbbaaaabbbbabaaabaaba
aabaaababbaabbaababbaabaabbaabbaababbbbb
bbaaaaabbbbaabbabbabbabaabaaaaaababaabab
aababaaabbbbaabbbbababbabbabaaaabaaaababbaaaababbabbbaabbaaabaab
aabbaaaaabbbaaaaabbaaaaabbbabaab
aababbbabbbaababaabababaababbbaaabaaabab
bbbaaaababaaabaaaaaabbabaababbbaabababaaaaaabaabaaababab
abaaaabbabababbaababbbab
bbabaabbaababababaabbbbb
babbabaabbabbbbbaaabaabbbabbaaaabaaabbba
aaaaabaababbabaaabbabbbb
bbaaaabbaabaaabbbaabbaba
abaaababaaabbbbbbbabbababbaaabaa
bbbbaababbbabaaababbabab
ababababaababaaabababbbbaaabbaababbbbabababaabba
babbaabaabaabbbbbaabaaaaaaabaabbbaabbabb
aaabaaaaababbaabaabbabaaabaabbabbbbbbaaabaaabaaa
baaabbaabaabaabbbaabbbab
aaaabbbaabaabbbbabbbbbaabbbbbbbbababbbaa
bbabbbaabababaababbaabaa
bbabaabaabbababaabaaaaabaaaaaaaabbbaabba
bbaababbbaabbaaabaaaaaba
baabbabbabbababbbbbbbbaabbbaaaaabaaaabbb
babaaabaaababbabbabaaaaaaaabbaaaaabbabbabaaabbbabbaabbab
babbabaaabaaaabaabbbababbaaaabba
abbababaaabaabbaabbbaaaabbaabaababbbbbabaabbbaaaaaababbaabbbbbbb
abbabbbaabaabaaaaababaabaaaabbbbabbaaaaababababbbbabbaabaabbababaaaabaabaaabababbbabbaab
bbaababbaaaaabaaaabbbaababbbbbab
babbbabbaaabaaababaabbbabbbaaaba
aaabaaabbaababbaabbbbbaaaabbbaba
bbaaabababaaaabaababaabaaabbbabbbbbbaabbbbaabbaababbbbabbbabbabbbababbab
abbbabbabbbbaaababbbbaabbaaaaaab
babbbabbaabbaabaaabbaaabbbbbaaabbaababbbaaaaaabb
aabbaabaabaabbabbbbbbbbabbbaabbb
abaaaaabbbbbaababaaaabab
abbbbbbaaabaaaaabbabaaaa
bbbbbbbaaabababaaaabaaba
bababbaababaaaaababaaaba
abbbaababaababbbbbbbabbabbbaabaa
abbbbbaabbbababababbbbbbbaaaabaabbbaaaba
abaaaabaabaaabbbbaababaa
bbaabaaaaaaaababaabbaabbabbbbbbabaabbbbabbababba
baabbaabaaaaabaabaabaabbbbabbbab
bbaaabbaaabbabbaaaaaaabbbbbaabbabbaaaaaabaabbbaa
aaaaaaabbbbbbbabbbababaabbababbb
abbbabbbbabbbaaaabababbb
aaabaaabaaaabbabaaaabbbb
babbaaaabbbbbbabbabbabba
ababababaabbabaabbabbbbbbaabbbaa
aabbbaabaaaaaaabaaababbb
aaaaabaaaaabbbbaaaaabbababaabbababaabababaaaabaa
abbbbaabbbabaabbababaaabaaabaaabbaaaabbabbaaabba
aaabbbbabbbbaabaababababbababbbabaabaabbbbbaabbb
abbbababbaababbababaabaa
ababaaababbbbaabbbabbabb
babbbbbbbaababbbbbbabaaaabababbbbaabbabb
aabaaabbaabbaaaabaaaabab
abbbabbbbababbbbbbabababababaabbaabbbbba
bbbabaaaaababaabaaaabbaa
bbbbaaabbabbaababbbaabba
ababababbaaababababaaaaaaaabababbaaabbbbbbaaabaaaaababbabaaaabba
bababbbbbbaaababaababaabaabbababbbbbbbba
baababbaaabaabababbaabba
bbbaababbbbbbbbabbaabbab
babbbbaababbbbbbbaabbbba
aaabaabbabbbbaabbbaababa
abbbaaaaaaaaabaabbabbaab
aabababbababaabaabbbbbbbaabbaababbaaaaaaaaabbbbaaaaabbaa
bababbbbaaababababbabababbbbbbbaababaaaaaaabbaaa
bbbbaaababbabbababababaa
bbbbbaabaaaababbaabbbbba
baabbaaabbbbabbbbbabaabaaabaabbbaabbbbbb
bababbaababbbbbbbaaaaabbbbaaabab
aabbaaabbbbbbbabaababbabbbaaabaa
bbbbaaabbaababbaaababbab
ababbaabaabbababbaaaabaa
baabaabbabbbabbbbaaaabbb
abbabbbabbbaababbbbaabba
abaaabbaaaabbabbababaabbbaaaababbaaabbbbaaabaabb
baabaabbbaaaaabbbabbaaaaaabbabbbbbbaabbb
abbaabababaabababbaababbabbaabaa
bbaaaaaabbaaaabaaabbababababbbab
aabbaaabbababbaabbbbabbbbaababababbaabba
bbbbaabbaababaababbbbaabbbabbbbaababbbaaabbbbbbbbbababbb
aaaaababaaaababaabbbababaabababaabbaaabbbabbbbab
aaabbbbabbabaabbbbaaabaa
bbabababbabaaabbbbbaaaba
aabbabbabbbabababaaabaab
bbbbaabbabbbabbbaabbbbbabaabababaaabbbab
baaabbbbaabbbabbbbaaaaaaabbbaabbaababbbb
bbaaabababbbaabaabbbaababababbabbbbabbab
bbbaaaababbaaababbbabbabbbbbbaaaababaabb
abaababbbabbbbabaababbabbaaaaabbbaababaaabbaabaabbababaaabaababb
ababbaabaabaabbaabbabaaa
bababbaababbaababaabbaabaabaaaaabaabaabaaababbabbbabaaaa
babaaaaaaabbbabbaaabbbaa
abbbbaabbbbbaababbbaababaaaaaaababababbb
bbabababbababbbbbbbbbaaa
aabbbaabbababbaaaabaaababbabbabb
baababbbabbbbbbabbbbbbbaaaabababbaaabbab
bababbbbabaabbaaabbbbaaabaaaabbb
abbaabaaabababbbaaaaaaaaaabbaabbbaabaabb
bbabbbbbbbabbbbabbbbaabbabaaaabbbaabbaba
bbaabbaabbababaabbaaababbabaaabb
bbaaaabbbaabaababbbbabbaababaabb
aaaaaaababaabbbabbbbabaa
bbbbaabaabababbabbaabaaabbababbaababbbbbaaaabbaabbbabbbabbaababb
abaaabbbbbbbababbbaababa
abaaaababbbbabbaabababba
aababbbabbbbaabbbababbbbabaaaaababbbaabbbababbab
bbbbaaaaabaabaabbbbbababbaaaaaba
babbaaaabaabbaabaaaaabba
bbbbaaaabaababbabbbaabba
ababababbbaaaaaaabbabaab
bababbbabbbaababaabbabaaabbabbbabbabbbab
baabaaabbabbbbbbbbbbbaabaabababa
ababbbbaabaaaabababbbbba
aaaaababbbabbbaababaaaba
baabababbaaaaabbbbababbb
bbbbabbabaabaaaababbbbba
aababaaabbabbbabbbbbbabbbbbaabbbbaabbabb
bbabaabaabbabababbbabaaaabaabaaababaaaba
aabbbbaaabbbbabbbabbabab
abaababaababbbbbbababbab
bbbabaaaabbbbabaaaabbabb
aabbaabbabbabbabbabbabba
bbababababaabbbbbabaaaab
bbbbaaaaaabaaabbbaabbbbb
aaaababbbaababababbabbaa
abaabbaaaabaaabababbbbba
aababbaabbaaababbbbbbbbababbaabb
babbbababbbbbaabbaabbbabbaaaababbbaabbabbbbaaabbaabbbabbbaababaa
bbaaaaabbaabaabaabbaaabbababaababaabaabbabbabbaabbaaabbababbbbab
abbabbbabaabaabbabbbaaab
babbababbaabbabaabaaabbabbababbbaaababbb
baababbbbbababaaababaaaabbabbaaa
bbbabaaaaaabbbaaaaaabbabbabababbabbbbaaaaaababab
ababaaabbaaabbbbababaaabbbbbaaaababbaabaabbbbabbbbabbaaa
abaaaaababbaaaaabbabbbaabaaaabaababaaaba
abaabababaabbaabbbbaaaab
babbbbbbbaaabbaaabbbbababbaaabbbabababaabbbaaaba
baabaaabaaaaaaabbbabaabbbbabbaba
aababbaaabbbaaaaabababba
abbbbabaabaabbaaaaaaaaababbbaaaabbabbaaababaaaab
bbaaaabbaababaabaabaaabaabbabbbb
bababaabbabbbaaabbaabaaa
bbbaabababbbbaabbabbabaabbaabaaa
babaaaaaabaaabbbbbababbb
abbbbbaabaabaabababbabba
baababbaaabbababababbbaa
aaabbbbaaaabababbabaabba
abaabbbabbbaabababaaabab
babbbaaaaabaaabababbbaba
abaabaabbabbaabaabaababb
aaaaaaabbabbaabaaaabbbab
bbabbbaabaabaababbaaaaab
bbaaaabbbbbbbbabababaabb
bbaababbbaaabbbabbaabbab
aaababababbabbabbabbabab
aabbaaaababaaaaaabbbabbaabaabbbaababaaaa
aabaaaaaabbaabababaaabab
baabaaabbbbababaababbbbb
ababababbbbbaababbaabaaa
bbbababaaababbbaabaaabab
bbbababbbbababababababbb
baababbbbbbbbbabaabbbbaabbbbbaaa
ababbbbaaabbaabbbbaabbbb
baababbababababbbbbbbbbaabbbbaabbbaaaaabbbaabbab
abaabbbabaababbaabbaabaa
aabbabbabbbbaaaaabaababb
baababbaabbaababaabaaaaabbbabbba
aabaaaaabbbbababbbbbaaaabbbabaabaababbab
aaaaaaaaabbaaabababbbbab
baaabbbbabbbaaaaababbbab
abaababaaababaaabababbaaabbbababbbabbbaaaaababbaaaaaabbaaaabbbbb
aabbbaaaabaabaaabbbbbbababbbbaaabbbabbaa
babbaaaaaabbbaaaabbbbbaaaabbabbabbbaabaa
bbbababbbabbbbaaaabaabaa
babbbabbabbbbbaaabbabbbb
aababaaabbaaaaababaaaaaabbbbbbababababbbbaaaabbabaababaababbabbaabbbbaaababababa
bbbabaaabaababbaaabaaaab
baabaababbabbbbbbaabbbbb
bbbbbbabaaababbbabbabbaabaabbbba
bababbabbaabbaaabaababbbabbbaababbabbaaaaababbabbbbbaaabbabbaaaaabaabbba
abbaaabbabbababaabaaabaa
bbaaababaabbabaaabaaabaa
bbabbbaaaaaaaaaaaaaabaab
bbaaaaabbbababbabbbaaabb
aaaaaaabababbabababaaaab
abbabbabbabbbbaabbabbabb
aabaaaaaabbbaaaaaabbabababaaabbbbbabaaabbabaabaabaaaabaa
bbbbbaabbbaaaaaababbbbbbaaaabbababbaabbbabbbbbabbaaaabab
aaabaaaaabbbabaabbaaaaba
aababaabaabbaaaabaaaaaaa
aaaabbbaaaabababaabbbaababbabaaabaaaaaaa
baabbaabbaabaabbbababbbbbbbabbaa
bbbaababbbbbaaaaaaaabaab
bababaabbabbabaabbabbaaa
baabbaaababaaaaabbaaaaaabbbabbbaaabbbaaabbbabaaaaababbbbbbbaaaab
abbaababbaababababaabbaaababbaababbaabbbaaaaaabb
babbabaaaaababbaabbaabaabbbbabba
babbaabbbabbaabbaaaabaab
aababbaaaaabaaabaabbbaabaabbabbaabbbbbbabbaabbbbbababaaa
aaaabbbababaaaaaaabaabbb
bbaababbabbbaaaaababaaabbbabbbbaaabbbbaababbababbbabbabb
aababbbaaaabaaaaabaababb
baaababbabbbbbbbbabaaabbabaabaabbbabbaba
abbabbabaaaaaabbaaabbabbbbbabaababbababbbbbaabba
abbbbabaaabbababbbbaaaba
abbbaababbabbabbbaabbabbbbabaaaa
abbbbaabaabbbabbaaaabbaa
aaabbbbaaaaabbababbaaabbaaaaabbb
bbbbaaabaaabaaabbaabbbaa
babbbabbbabbabaaabbabbbb
ababbbbaabbbabbbaaabbaaa
bbabbbbbababbaabaaaabbbb
bbbababbbbbbbbbabbbabbbb
abbbabbbbababbbbabbabbbb";

/// Enum where each variant describes a rule
///
/// Literal: The rule matches a string literal. Can be 1 or more characters.
/// ReferenceAnotherRule: The rule matches if the rule referenced does
/// RuleSequence: The rule matches only if all child rules match in succession
/// OneOfSeveralSequences: The rule matches if one or more child rules match
#[derive(Debug)]
enum Rule {
    Literal(String),
    ReferenceAnotherRule(u32),
    RuleSequence(Vec<Rule>),
    OneOfSeveralSequences(Vec<Rule>),
}

/// Parse a rule string such as `1 2 3` into a `Rule::RuleSeqence`
fn parse_muliple_rule_str(refs_to_other_rules: &str) -> Rule {
    let mut ret_subrules = Vec::new();
    for sub_rule in refs_to_other_rules.trim().split(" ") {
        ret_subrules.push(Rule::ReferenceAnotherRule(sub_rule.parse().unwrap()));
    }
    return Rule::RuleSequence(ret_subrules);
}

/// Parse the rules portion of the input into a map from the rule ID to the actual rule
fn parse_rules(rules_str: &str) -> HashMap<u32, Rule> {
    let mut ret = HashMap::new();

    for rule_line in rules_str.split("\n") {
        let rule_idx_and_contents: Vec<&str> = rule_line.split(": ").collect();
        let rule_idx: u32 = rule_idx_and_contents.get(0).unwrap().parse().unwrap();
        let rule_contents: &str = rule_idx_and_contents.get(1).unwrap();
        if rule_contents.contains("|") {
            ret.insert(
                rule_idx,
                Rule::OneOfSeveralSequences(
                    rule_contents
                        .split(" | ")
                        .map(&parse_muliple_rule_str)
                        .collect(),
                ),
            );
        } else if rule_contents.contains("\"") {
            let literal = rule_contents.replace("\"", "").chars().collect();
            ret.insert(rule_idx, Rule::Literal(literal));
        } else {
            ret.insert(rule_idx, parse_muliple_rule_str(rule_contents));
        }
    }

    return ret;
}

/// Return a HashSet containing just one item, which is `item`
fn singleton_set<T: Eq + std::hash::Hash>(item: T) -> HashSet<T> {
    let mut ret = HashSet::new();
    ret.insert(item);
    return ret;
}

/// ret val is
/// - none if rule did not match
/// - the number of characters matched (wrapped in Some) if it did match
fn does_rule_match(
    rules_map: &HashMap<u32, Rule>,
    rule: &Rule,
    string: &str,
) -> Option<HashSet<usize>> {
    // println!("\n\nmatching str {} to rule: {}\n{:?}", string, rule, rule);
    match rule {
        Rule::Literal(literal) => {
            if string.starts_with(literal) {
                // println!("matched");
                Some(singleton_set(literal.len()))
            } else {
                // println!("didn't match");
                None
            }
        }
        Rule::ReferenceAnotherRule(other_rule_num) => {
            does_rule_match(rules_map, rules_map.get(&other_rule_num).unwrap(), string)
        }
        Rule::RuleSequence(subrules) => {
            let mut possible_idxes: HashSet<usize> = singleton_set(0);
            for subrule in subrules.iter() {
                let mut new_possible_idxes = HashSet::new();
                for idx in possible_idxes.iter() {
                    let subrule_match_result =
                        does_rule_match(rules_map, subrule, &string[*idx..string.len()]);

                    if let Some(num_chars_matched) = subrule_match_result {
                        new_possible_idxes.extend(num_chars_matched.iter().map(|x| x + idx));
                    }
                }
                possible_idxes = new_possible_idxes;
            }

            if possible_idxes.len() == 1 && possible_idxes.get(&0).is_some() {
                return None;
            }
            return Some(possible_idxes);
        }
        Rule::OneOfSeveralSequences(subrules) => {
            let sub_rule_matches: HashSet<usize> = subrules
                .iter()
                .map(|subrule| does_rule_match(rules_map, subrule, string))
                .filter_map(|res| res)
                .flat_map(|res| res)
                .collect();

            if sub_rule_matches.len() == 0 {
                return None;
            }
            Some(sub_rule_matches)
        }
    }
}

impl Clone for Rule {
    fn clone(&self) -> Self {
        match self {
            Rule::Literal(chr) => Rule::Literal(chr.clone()),
            Rule::ReferenceAnotherRule(i) => Rule::ReferenceAnotherRule(*i),
            Rule::OneOfSeveralSequences(seqs) => Rule::OneOfSeveralSequences(seqs.clone()),
            Rule::RuleSequence(seqs) => Rule::RuleSequence(seqs.clone()),
        }
    }
}

fn collapse_rule(rule: Rule) -> Rule {
    match rule {
        Rule::OneOfSeveralSequences(seqs) => {
            Rule::OneOfSeveralSequences(seqs.iter().cloned().map(&collapse_rule).collect())
        }
        Rule::RuleSequence(seqs) => {
            let mut collapsed_subrules = Vec::new();
            let mut buffer = String::new();
            for subrule in seqs {
                match subrule {
                    Rule::Literal(literal) => buffer.push_str(literal.as_str()),
                    other => {
                        if buffer.len() > 0 {
                            collapsed_subrules.push(Rule::Literal(buffer.clone()));
                            buffer.clear();
                        }
                        collapsed_subrules.push(other);
                    }
                }
            }

            if buffer.len() > 0 {
                collapsed_subrules.push(Rule::Literal(buffer.clone()));
                buffer.clear();
            }

            Rule::RuleSequence(collapsed_subrules)
        }
        other => other,
    }
}

/// Condense the rules by
/// 1. Inlining rules. For example,
/// ```
/// 1: 2 3
/// 2: 3 | "b"
/// 3: "a"
/// ```
///
/// would become
///
/// ```
/// 1: ("a" | "b") "a"
/// 2: 3 | "b"
/// 3: "a"
/// ```
///
/// after enough repeated applications of this function on rule `1`.
///
/// Returns Some if the could be condensed, None otherwise
fn condense_rule(rules_map: &HashMap<u32, Rule>, rule: &Rule) -> Option<Rule> {
    // println!("{:?}", rule);
    match rule {
        Rule::ReferenceAnotherRule(i) => {
            if *i != 8 && *i != 11 {
                Some(rules_map.get(i).unwrap().clone())
            } else {
                None
            }
        }
        Rule::OneOfSeveralSequences(subrules) => {
            let mut new_subrules: Vec<Rule> = Vec::new();
            let mut changed_anything = false;
            for subrule in subrules.iter() {
                let maybe_condensed_subrule = condense_rule(rules_map, subrule);
                if let Some(condensed_subrule) = maybe_condensed_subrule {
                    new_subrules.push(condensed_subrule);
                    changed_anything = true;
                } else {
                    new_subrules.push(subrule.clone());
                }
            }
            if changed_anything {
                Some(Rule::OneOfSeveralSequences(new_subrules))
            } else {
                None
            }
        }
        Rule::RuleSequence(subrules) => {
            let mut new_subrules: Vec<Rule> = Vec::new();
            let mut changed_anything = false;
            for subrule in subrules.iter() {
                let maybe_condensed_subrule = condense_rule(rules_map, subrule);
                if let Some(condensed_subrule) = maybe_condensed_subrule {
                    new_subrules.push(collapse_rule(condensed_subrule));
                    changed_anything = true;
                } else {
                    new_subrules.push(collapse_rule(subrule.clone()));
                }
            }
            if changed_anything {
                Some(Rule::RuleSequence(new_subrules))
            } else {
                None
            }
        }
        Rule::Literal(_) => None, // string literal is as condensed as it can get
    }
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Rule::Literal(chr) => write!(f, "\"{}\"", chr),
            Rule::ReferenceAnotherRule(i) => write!(f, "{}", i),
            Rule::OneOfSeveralSequences(seqs) => write!(
                f,
                "({})",
                seqs.iter()
                    .map(|subrule| subrule.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            Rule::RuleSequence(seqs) => write!(
                f,
                "{}",
                seqs.iter()
                    .map(|subrule| subrule.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

/// Count the number of messages that fully match rule 0 in the provided rule set
/// Parameters:
///     - rules: Map from rule number to Rule
///     - messages: newline-separated list of messages to match
fn count_matching_messages(rules: &HashMap<u32, Rule>, messages: &str) -> u32 {
    let rule_0 = rules.get(&0).unwrap();
    let mut matching_messages_count = 0;
    for msg in messages.split("\n") {
        let match_result = does_rule_match(&rules, rule_0, msg);
        if let Some(chars_matched) = match_result {
            if chars_matched.get(&msg.len()).is_some() {
                matching_messages_count += 1;
            }
        }
    }
    return matching_messages_count;
}

pub fn day19_main() {
    let input: (&str, &str) = include_str!("../../inputs/day19.txt")
        .trim_end()
        .split("\n\n")
        .collect_tuple()
        .unwrap();
    let rules = input.0.trim();
    let messages = input.1.trim();

    let parsed_rules = parse_rules(rules);

    let mut condensed_rules: HashMap<u32, Rule> =
        parsed_rules.iter().map(|(a, b)| (*a, b.clone())).collect();
    loop {
        let mut changed_anything = false;
        let mut new_rules = HashMap::new();
        for (rule_num, rule) in condensed_rules.iter() {
            let mby_rule = condense_rule(&condensed_rules, rule);
            if let Some(condensed) = mby_rule {
                new_rules.insert(*rule_num, condensed);
                changed_anything = true;
            } else {
                new_rules.insert(*rule_num, rule.clone());
            }
        }
        if changed_anything {
            condensed_rules = new_rules;
        } else {
            break;
        }
    }

    println!(
        "Part 1: {} messages fully matched the rules",
        count_matching_messages(&condensed_rules, messages)
    );

    // Part 2
    // manually update rules 8, 11

    // 8: 42 | 42 8
    condensed_rules.insert(
        8,
        Rule::OneOfSeveralSequences(vec![
            Rule::ReferenceAnotherRule(42),
            Rule::RuleSequence(vec![
                Rule::ReferenceAnotherRule(42),
                Rule::ReferenceAnotherRule(8),
            ]),
        ]),
    );

    // 11: 42 31 | 42 11 31
    condensed_rules.insert(
        11,
        Rule::OneOfSeveralSequences(vec![
            Rule::RuleSequence(vec![
                Rule::ReferenceAnotherRule(42),
                Rule::ReferenceAnotherRule(31),
            ]),
            Rule::RuleSequence(vec![
                Rule::ReferenceAnotherRule(42),
                Rule::ReferenceAnotherRule(11),
                Rule::ReferenceAnotherRule(31),
            ]),
        ]),
    );

    println!(
        "Part 2: {} messages fully matched the rules",
        count_matching_messages(&condensed_rules, messages)
    );
}
