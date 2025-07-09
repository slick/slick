# Slick Distributed Tracing

This module provides OpenTelemetry-based distributed tracing capabilities for Slick, enabling comprehensive monitoring and observability of database operations.

## Features

- **OpenTelemetry Integration**: Full compliance with OpenTelemetry database semantic conventions
- **SQL Comment Injection**: Automatic injection of trace context and application metadata into SQL queries for cloud database insights
- **Query Compilation Tracing**: Detailed instrumentation of query compilation phases
- **Connection Pool Monitoring**: Real-time metrics for database connection lifecycle
- **Streaming Query Tracing**: Performance monitoring for streaming database operations
- **Cloud Database Support**: Native integration with Google Cloud SQL, AWS Aurora, and Azure SQL

## Quick Start

### 1. Add Dependencies

```scala
libraryDependencies += "com.typesafe.slick" %% "slick-tracing" % "3.7.0"
```

### 2. Initialize OpenTelemetry

```scala
import io.opentelemetry.api.OpenTelemetry
import slick.tracing._

// Initialize OpenTelemetry (implementation specific)
val openTelemetry: OpenTelemetry = // ... your OpenTelemetry setup

// Create tracing profile
val profile = TracingJdbcProfile.withOpenTelemetry(openTelemetry)
```

### 3. Use Tracing Profile

```scala
import profile.api._

// Define your table
class Users(tag: Tag) extends Table[(Long, String)](tag, "users") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def * = (id, name)
}

val users = TableQuery[Users]

// Queries are automatically traced
val query = users.filter(_.name === "Alice").result

// Add custom tracing information
val tracedQuery = users.filter(_.name === "Alice")
  .withTracing(Map(
    "operation" -> "user-lookup",
    "component" -> "user-service",
    "version" -> "1.0"
  ))
  .result
```

## Configuration

Configure tracing via `application.conf`:

```hocon
slick.tracing {
  enabled = true
  
  query-execution {
    enabled = true
    include-parameters = false
    include-resultset-metadata = true
    connection-pool-metrics = true
    streaming-metrics = true
  }
  
  query-compilation {
    enabled = true
    include-phase-details = false
    performance-metrics = true
  }
  
  sql-comments {
    enabled = true
    include-trace-context = true
    include-application-tags = true
    format = "standard"
  }
  
  cloud-integration {
    google-cloud-sql {
      enabled = true
      project-id = "my-project"
      instance-id = "my-instance"
      query-insights = true
    }
    
    aws-aurora {
      enabled = true
      cluster-identifier = "my-cluster"
      performance-insights = true
    }
    
    azure-sql {
      enabled = true
      server-name = "my-server"
      query-store = true
    }
  }
}
```

## Cloud Database Integration

### Google Cloud SQL

The module automatically injects SQL comments compatible with Google Cloud SQL Query Insights:

```sql
SELECT * FROM users WHERE name = 'Alice' /*traceparent='00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01', db_driver='slick', operation='user-lookup'*/
```

### AWS Aurora

For AWS Aurora Performance Insights, configure the AWS integration:

```hocon
slick.tracing.cloud-integration.aws-aurora {
  enabled = true
  cluster-identifier = "my-aurora-cluster"
  performance-insights = true
}
```

### Azure SQL

For Azure SQL Query Store integration:

```hocon
slick.tracing.cloud-integration.azure-sql {
  enabled = true
  server-name = "my-sql-server"
  query-store = true
}
```

## Advanced Usage

### Custom Tracing Context

```scala
import io.opentelemetry.api.OpenTelemetry
import slick.tracing._

// Create custom tracing context
val tracingContext = new DefaultTracingContext(
  openTelemetry = openTelemetry,
  isTracingEnabled = true
)

// Create profile with custom context
val profile = new DefaultTracingJdbcProfile(
  tracingConfig = TracingConfig.fromConfig(),
  tracingContext = tracingContext
)
```

### Database-Specific Profiles

```scala
import slick.tracing.profiles._

// PostgreSQL with tracing
val postgresProfile = postgres()

// MySQL with tracing
val mysqlProfile = mysql()

// H2 with tracing
val h2Profile = h2()
```

### Manual Tracing

```scala
import profile.api._

// Create traced database action
val tracedAction = traced("custom-operation") {
  // Your database operation
  users.filter(_.name === "Alice").result
}

// Execute with tracing
database.run(tracedAction)
```

## Performance Considerations

The tracing module is designed for minimal performance impact:

- **Configurable Sampling**: Control trace sampling rates
- **Asynchronous Processing**: Non-blocking span creation and processing
- **Efficient SQL Injection**: Minimal overhead for comment injection
- **Optional Detailed Tracing**: Disable expensive features in production

### Sampling Configuration

```hocon
slick.tracing.performance {
  sampling-rate = 0.1  # Trace 10% of operations
  max-spans-per-trace = 50
  async-processing = true
}
```

## OpenTelemetry Semantic Conventions

The module follows OpenTelemetry semantic conventions for database operations:

| Attribute | Description | Example |
|-----------|-------------|---------|
| `db.system` | Database system | `postgresql`, `mysql` |
| `db.name` | Database name | `myapp_production` |
| `db.statement` | SQL statement (sanitized) | `SELECT * FROM users WHERE id = ?` |
| `db.user` | Database user | `app_user` |
| `server.address` | Server address | `localhost`, `db.example.com` |
| `server.port` | Server port | `5432`, `3306` |
| `db.operation.name` | Operation name | `users.select`, `users.insert` |

## Security Considerations

### SQL Statement Sanitization

By default, the module sanitizes SQL statements to prevent sensitive data exposure:

```scala
// Original query
"SELECT * FROM users WHERE name = 'Alice' AND password = 'secret123'"

// Sanitized for tracing
"SELECT * FROM users WHERE name = ? AND password = ?"
```

### Parameter Sanitization

Parameter values are not included in traces by default. To include them:

```hocon
slick.tracing.query-execution.include-parameters = true
```

**Warning**: Only enable parameter logging in non-production environments.

## Troubleshooting

### Tracing Not Working

1. Verify OpenTelemetry is properly initialized
2. Check configuration: `slick.tracing.enabled = true`
3. Ensure spans are being exported to your tracing backend

### Performance Issues

1. Reduce sampling rate: `slick.tracing.performance.sampling-rate = 0.1`
2. Disable detailed tracing: `slick.tracing.query-compilation.include-phase-details = false`
3. Enable async processing: `slick.tracing.performance.async-processing = true`

### SQL Comments Not Appearing

1. Check cloud integration configuration
2. Verify SQL comment format matches your database
3. Ensure `slick.tracing.sql-comments.enabled = true`

## License

This module is part of Slick and is released under the same license as the main Slick project.