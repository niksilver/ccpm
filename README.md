A playground for exploring critical chain project management
via a Scala DSL.

You can add tasks to a plan, and state dependencies between them.

```scala
    new Plan {
      add task 't0 as "Assemble the team"
      add task 't1 as "Brief the team"
      add task 't2 as "Be awesome"
      't0 ~> 't1 ~> 't2
    }
```