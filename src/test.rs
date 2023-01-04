
    use nom::error::VerboseError;

    use crate::parser::{
        procedure::{procedure, Procedure, Instruction},
        Fragment,
    };

    #[test]
    fn procedure_test() {
        let proc = procedure::<VerboseError<&str>>("procedure Day");

        assert_eq!(proc.unwrap().1, Procedure::named("Day"))
    }

    #[test]
    fn procedure_body() {
        let proc = procedure::<VerboseError<&str>>("procedure Day -> say: \"Hello, world!\"");

        println!("{proc:?}");

        assert_eq!(
            proc.unwrap().1,
            Procedure::with_body(
                "Day",
                vec![Instruction::with_arguments(
                    "say",
                    vec![Fragment::String("Hello, world!".into())]
                )]
            )
        )
    }

    #[test]
    fn wrong_procedure() {
        let proc = procedure::<VerboseError<&str>>("procedure ()");
        assert_eq!(proc.is_err(), true)
    }