.__C__linreg <-
new("refClassRepresentation", fieldClasses = list(formula = "formula", 
    data = "data.frame", Y = "numeric", X = "matrix", beta_hat = "matrix", 
    y_hat = "array", e = "array", df = "numeric", sigma_2 = "matrix", 
    VAR_B = "matrix", t_b = "array", p_values = "array"), fieldPrototypes = <environment>, 
    refMethods = <environment>, refSuperClasses = "envRefClass", 
    slots = list(.xData = structure("environment", package = "methods")), 
    contains = list(envRefClass = new("SClassExtension", subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("envRefClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            class(from) <- "envRefClass"
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            for (what in ".xData") slot(from, what) <- slot(value, 
                what)
            from
        }, simple = TRUE, by = character(0), dataPart = FALSE, 
        distance = 1), .environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure(".environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            {
                class(from) <- ".environment"
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, ".environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), refClass = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refClass") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- as(from, "envRefClass", strict = strict)
            {
                from <- as(from, ".environment", strict = strict)
                from@.xData
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = FALSE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3), refObject = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refObject", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refObject") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3)), virtual = FALSE, prototype = <S4 object of class NULL>, 
    validity = NULL, access = list(), className = structure("linreg", package = ".GlobalEnv"), 
    package = ".GlobalEnv", subclasses = list(), versionKey = <pointer: (nil)>, 
    sealed = FALSE)
.__C__ridgereg <-
new("refClassRepresentation", fieldClasses = list(formula = "formula", 
    data = "data.frame", lambda = "numeric", Y = "numeric", X = "matrix", 
    Xnorm = "matrix", beta_hat = "matrix", y_hat = "array", df_name = "character"), 
    fieldPrototypes = <environment>, refMethods = <environment>, 
    refSuperClasses = "envRefClass", slots = list(.xData = structure("environment", package = "methods")), 
    contains = list(envRefClass = new("SClassExtension", subClass = structure("ridgereg", package = ".GlobalEnv"), 
        superClass = structure("envRefClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            class(from) <- "envRefClass"
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            for (what in ".xData") slot(from, what) <- slot(value, 
                what)
            from
        }, simple = TRUE, by = character(0), dataPart = FALSE, 
        distance = 1), .environment = new("SClassExtension", 
        subClass = structure("ridgereg", package = ".GlobalEnv"), 
        superClass = structure(".environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            {
                class(from) <- ".environment"
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, ".environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), refClass = new("SClassExtension", 
        subClass = structure("ridgereg", package = ".GlobalEnv"), 
        superClass = structure("refClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refClass") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), environment = new("SClassExtension", 
        subClass = structure("ridgereg", package = ".GlobalEnv"), 
        superClass = structure("environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- as(from, "envRefClass", strict = strict)
            {
                from <- as(from, ".environment", strict = strict)
                from@.xData
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = FALSE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3), refObject = new("SClassExtension", 
        subClass = structure("ridgereg", package = ".GlobalEnv"), 
        superClass = structure("refObject", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refObject") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3)), virtual = FALSE, prototype = <S4 object of class NULL>, 
    validity = NULL, access = list(), className = structure("ridgereg", package = ".GlobalEnv"), 
    package = ".GlobalEnv", subclasses = list(), versionKey = <pointer: 0x00007ffc7f8f4d90>, 
    sealed = FALSE)
.__global__ <-
c("formula", "data", "Y", "X", "beta_hat", "y_hat", "e", "df", 
"sigma_2", "VAR_B", "t_b", "p_values", "df_name", "summary", 
"coef", "pred", "resid", "print", "plot", "initialize", "field", 
"trace", "getRefClass", "initFields", "copy", "callSuper", ".objectPackage", 
"export", "untrace", "getClass", "show", "usingMethods", ".objectParent", 
"import", ".self", "call", "lambda", "Xnorm", "show#envRefClass"
)
.requireCachedGenerics <-
structure(list("$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", "$", "$<-", 
    "$", "$<-"), package = c("base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base", "base", "base", 
"base", "base", "base", "base", "base", "base"))
