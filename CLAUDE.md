# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Slick is a functional relational mapping (FRM) library for Scala that provides type-safe, composable database access.
It's a multi-module SBT project with the following key modules:

- **slick**: Core library with lifted embedding, AST, compiler, and JDBC support
- **slick-testkit**: Comprehensive test suite and database compatibility testing
- **slick-codegen**: Code generation from database schemas
- **slick-hikaricp**: HikariCP connection pool integration
- **reactive-streams-tests**: Reactive Streams compliance tests

## Common Development Commands

### Building and Compilation

```bash
# Clean and compile
sbt clean compile

# Check dependencies
sbt dependencyUpdates

# Generate API documentation
sbt doc
```

## Architecture Overview

### Core Layers

1. **AST Layer** (`slick.ast`): Immutable query representation with a comprehensive type system
2. **Compiler Layer** (`slick.compiler`): Multi-phase query compilation pipeline
3. **Lifted Embedding** (`slick.lifted`): Type-safe DSL for database queries
4. **Profile System**: Database-specific implementations (H2Profile, PostgresProfile, etc.)
5. **Backend/JDBC** (`slick.jdbc`): SQL generation and database execution

### Key Components

- **QueryCompiler**: Immutable, configurable compilation pipeline
- **Profile**: Database abstraction layer with a capability system
- **DBIOAction**: Monadic database operations with async support
- **Table/Query**: Core abstractions for database schema and queries
- **Rep[T]**: Lifted representation of database values

### Database Support

Slick supports multiple databases through profile-based architecture:

- PostgreSQL, MySQL, SQL Server, Oracle, DB2
- H2, Derby, HSQLDB, SQLite (embedded databases)
- Each database has specific profile (e.g., `PostgresProfile`, `H2Profile`)

## Code Organization

### Main Source Structure

```
slick/src/main/scala/slick/
├── ast/           # Abstract syntax tree and type system
├── compiler/      # Query compilation phases
├── lifted/        # Type-safe query DSL
├── jdbc/          # JDBC backend and profiles
├── basic/         # Basic profile abstraction
├── relational/    # Relational model abstractions
├── sql/           # SQL-specific functionality
├── memory/        # In-memory database support
├── dbio/          # Database I/O actions
└── util/          # Utilities and helpers
```

### Test Structure

```
slick-testkit/src/
├── main/scala/    # Testkit framework and utilities
├── test/scala/    # Unit tests and benchmarks
├── codegen/       # Code generation tests
└── doctest/       # Documentation tests
```

## Testing Framework

### Test Categories

- **Unit Tests**: Basic functionality and edge cases
- **Database Tests**: Cross-database compatibility (grouped by database type)
- **Compile Tests**: Compile-time behavior verification
- **Integration Tests**: End-to-end scenarios
- **Performance Tests**: Benchmarks and stress tests
- **Documentation Tests**: Code examples in documentation
- **Reactive Streams Tests**: Streaming compliance tests

### Test Configuration System

Tests use Typesafe Config for database configuration:

- **testkit-reference.conf**: Defines test database configurations and test classes
- **Multi-database support**: PostgreSQL, MySQL, Oracle, DB2, SQL Server, H2, Derby, HSQLDB, SQLite
- **Test grouping**: Database-specific test groups with controlled concurrency
- **Environment-specific configs**: GitHub Actions configurations in `test-dbs/`
- **Key test databases**: H2 (default for fast testing), PostgreSQL, MySQL (for production database testing)

### Running Tests

```bash
# Run all tests (includes testkit, doctests, and reactive-streams tests)
sbt testAll

# Run only testkit tests
sbt testkit/test

# Run documentation tests
sbt testkit/DocTest/test

# Run reactive streams tests
sbt reactive-streams-tests/test

# Test specific database
sbt -Dslick.dbs=postgres testkit/test

# Test specific functionality
sbt "testkit/testOnly *JoinTest*"

# Run specific test with pattern
sbt "testkit/testOnly slick.test.profile.H2MemTest -- -z com.typesafe.slick.testkit.tests.JoinTest.testJoin[h2mem]"

# Run with debug output
sbt -Dslick.ansiDump=true testkit/test

# Run with SQL debugging
sbt -Dslick.ansiDump=true testkit/test
```

### Local Database Testing with Docker

```bash
# Start databases using docker-compose
docker compose up -d postgres
docker compose up -d mysql

# Test specific database with proper configuration
env SLICK_TESTKIT_CONFIG=test-dbs/testkit.github-actions.conf sbt \
  -Ddb2.enabled=false -Dmysql.enabled=false -Doracle.enabled=false \
  -Dsqlserver-sqljdbc.enabled=false -Dpostgres.enabled=true \
  "testkit/testOnly slick.test.profile.PostgresTest"

# Wait for database readiness
docker compose exec postgres pg_isready -U postgres
```

### Test Development Patterns

- **ExternalJdbcTestDB**: Base class for database-specific test configurations
- **ResultSetAction**: Use for custom metadata queries with proper connection context
- **Database cleanup**: Override `dropUserArtifacts` for database-specific cleanup logic
- **CI configuration**: Use `test-dbs/testkit.github-actions.conf` for CI-specific database settings

## Development Guidelines

### Code Style

- Follow Scala best practices and existing codebase patterns
- Leverage the type system for compile-time safety
- Write comprehensive tests for new features

### Database Compatibility

- New features should work across supported databases
- Use capability system to handle database-specific features
- Test against multiple database profiles
- Handle database-specific SQL generation in profiles

### Performance Considerations

- Compiler phases are optimized for query performance
- Use streaming for large result sets
- Async operations with proper resource management

## Common Issues and Solutions

### Compilation Problems

- Type inference issues: Check `slick.ast.Type` system
- Implicit resolution: Review `Shape` and `TypedType` implicits
- Compiler phases: Debug with `QueryCompiler` logging

### Database Testing Issues

- Connection issues: Check database configuration in `application.conf`
- SQL generation: Enable SQL logging with `slick.ansiDump=true`
- Database-specific features: Verify profile capabilities
- **PostgreSQL JDBC Compatibility**: When upgrading PostgreSQL JDBC drivers, catalog parameter handling may change. Use
  `conn.getCatalog` instead of empty strings in `DatabaseMetaData.getTables()` calls
- **"Relation already exists" errors**: Usually indicates database cleanup issues between tests. Check
  `dropUserArtifacts` implementations in test database configurations

### Performance Issues

- Large queries: Use streaming with `DatabasePublisher`
- Connection pooling: Configure HikariCP appropriately
- Query optimization: Review compiled SQL output

## Key Files to Understand

### Core Architecture

- `slick/ast/Node.scala`: Base AST node with a type system
- `slick/compiler/QueryCompiler.scala`: Compilation pipeline
- `slick/lifted/Query.scala`: Main query interface
- `slick/jdbc/JdbcProfile.scala`: JDBC backend implementation

### Database Profiles

- `slick/jdbc/H2Profile.scala`: H2 database support
- `slick/jdbc/PostgresProfile.scala`: PostgreSQL support
- `slick/basic/BasicProfile.scala`: Base profile abstraction

### Testing Infrastructure

- `slick-testkit/src/main/scala/com/typesafe/slick/testkit/util/TestDB.scala`: Test database management
- `slick-testkit/src/main/scala/com/typesafe/slick/testkit/util/StandardTestDBs.scala`: Standard test configurations
- `test-dbs/testkit.github-actions.conf`: CI-specific database configurations
- `docker-compose.yml`: Local database setup for testing

## Build System and Module Structure

### SBT Configuration

- **Multi-module project**: Root project aggregates slick, codegen, hikaricp, testkit, and site modules
- **Cross-compilation**: Supports Scala 2.12, 2.13, and 3.x
- **Custom tasks**: `testAll` runs comprehensive test suite across all modules
- **Dependencies**: Managed in `project/Dependencies.scala` with version centralization
- **Test concurrency**: Configured with test group tags to limit concurrent database tests (PostgreSQL, DB2, etc.)
- **Forked tests**: Database tests run in forked JVMs for isolation

### Module Dependencies

- **slick**: Core library, no dependencies on other modules
- **slick-testkit**: Depends on slick, codegen, hikaricp for comprehensive testing
- **slick-codegen**: Depends on slick for database schema code generation
- **slick-hikaricp**: Depends on slick for connection pooling integration
- **reactive-streams-tests**: Depends on testkit for streaming compliance tests


## Compiler Pipeline Architecture

### Query Compilation Phases

The `QueryCompiler` uses an immutable, configurable pipeline:

1. **AssignUniqueSymbols**: Symbol resolution and uniqueness
2. **InferTypes**: Type inference and validation
3. **MergeToComprehensions**: Query optimization
4. **RewriteJoins**: Join transformation and optimization
5. **Database-specific phases**: SQL generation for the target database

### Compilation Customization

- **Profile-specific compilers**: Each database profile has customized compilation
- **Phase modification**: Phases can be added, removed, or replaced
- **Debug support**: Compilation logging and AST dumping capabilities

## Advanced Development Patterns

### AST Manipulation

- **Immutable nodes**: All AST nodes are immutable with structural sharing
- **Type system**: Comprehensive type annotations throughout the AST
- **Node transformation**: Pattern matching and tree rewriting for optimizations

### Database Profile System

- **Capability-based**: Each profile declares supported database features
- **SQL generation**: Profile-specific SQL builders and formatters
- **Type mapping**: Database-specific type conversions and encodings

### Performance Considerations

- **Streaming support**: Reactive Streams integration for large result sets
- **Connection pooling**: HikariCP integration with monitoring support
- **Async operations**: Future-based API with proper resource management
- **Compilation caching**: Prepared statement caching and reuse

## Critical Development Notes

### Database Driver Compatibility

- **JDBC driver upgrades**: Always test against multiple database versions when updating drivers
- **Metadata API changes**: DatabaseMetaData behavior can change between driver versions, especially catalog parameter
  handling
- **Test isolation**: Database cleanup between tests relies on proper metadata queries
