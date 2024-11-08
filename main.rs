// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    pub executable: String,
    #[builder(each = "arg")]
    pub args: Vec<String>,
    #[builder(each = "env")]
    pub env: Vec<String>,
    pub current_dir: Option<String>,
}

fn main() {}
