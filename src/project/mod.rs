use std::{path::Path, fs::read_to_string, };

#[derive(Debug, knuffel::Decode)]
pub struct ProjectFile {
    #[knuffel(child, unwrap(argument))]
    pub name: String,
    #[knuffel(child, unwrap(argument))]
    pub description: String,
    #[knuffel(child, unwrap(argument))]
    pub version: String,
}

#[derive(Debug)]
pub struct Project {
    pub data: ProjectFile,
}

impl Project {
    fn new(data: ProjectFile) -> Self { Self { data } }
}

pub fn parse_project(root: &String) -> Project {
    let root = Path::new(&root);

    let path= root.join(Path::new("project.kdl"));
    let pf = read_to_string(path.to_str().unwrap()).unwrap();

    let project = match knuffel::parse::<ProjectFile>(path.to_str().unwrap(), &pf) {
        Ok(project) => project,
        Err(e) => {
             println!("{:?}", miette::Report::new(e));
             std::process::exit(1);
        }
    };

    Project::new(project)
}