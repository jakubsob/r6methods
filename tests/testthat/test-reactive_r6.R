describe("ReactiveR6", {
  TestClass <- R6::R6Class(
    "TestClass",
    inherit = ReactiveR6,
    public = list(
      increment = function() {
        private$counter <- private$counter + 1
        private$invalidate()
      },
      get_counter = function() {
        private$counter
      }
    ),
    private = list(
      counter = 0
    )
  )

  it("should become a reactive when `reactive` method is called", {
    test_class <- TestClass$new()
    expect_equal(class(test_class$reactive()), c("reactiveExpr", "reactive", "function"))
  })

  it("should trigger an observer watching the class after `invalidate` method has been called", {
    observer <- mockery::mock()
    shiny::testServer(
      app = function(input, output, session) {
        test_class <- TestClass$new()$reactive()
        shiny::observe({
          isolate(test_class()$increment())
        })

        shiny::observeEvent(test_class(), {
          observer()
        })
      }, {
        expect_equal(test_class()$get_counter(), 0)
        mockery::expect_called(observer, n = 0)

        session$flushReact()

        expect_equal(test_class()$get_counter(), 1)
        mockery::expect_called(observer, n = 1)
      }
    )
  })
})

