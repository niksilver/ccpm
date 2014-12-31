A playground for exploring critical chain project management
via a Scala DSL.

You can add tasks to a plan, and state dependencies between them.

```scala
    new Plan {
      add task 't0  as "Assemble the team"
      add task 't1a as "Brief the team" duration 4
      add task 't1b as "Be awesome"
      add task 'end
      't0 ~> 't1a ~> 't1b
      't1b ~> 'end
    }
```