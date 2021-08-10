{ port = 8080
, jwtSecret = "b66e721f-85a5-482d-9e34-1eb3c748c418"
, poolSize = 4
, db =
    { port = 5432
    , host = "localhost"
    , user = "postgres"
    , passwd = "password"
    , database = "conduit"
    }
}
