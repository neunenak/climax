_default:
    @just --list


# Execute the climax-example binary
exec *args:
    lake exec climax-example {{args}}
