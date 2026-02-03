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
# Clean and compile all modules
sbt clean compile

# Cross-compile for all supported Scala versions
sbt ++2.12.20 compile
sbt ++2.13.16 compile
sbt ++3.3.4 compile

# Force specific Scala version for testing
sbt ++3.3.4! compile

# Generate API documentation
sbt doc
```

### Testing Commands

```bash
# Run all tests (includes testkit, doctests, and reactive-streams tests)
sbt testAll

# Run only testkit tests
sbt testkit/test

# Run documentation tests
sbt testkit/DocTest/test

# Run reactive streams tests  
sbt reactive-streams-tests/test

# Test specific database profiles
sbt "testkit/testOnly slick.test.profile.H2MemTest"
sbt "testkit/testOnly slick.test.profile.PostgresTest"

# Test with pattern matching
sbt "testkit/testOnly *JoinTest*"
sbt "testkit/testOnly *MainTest* -- -z insertTest"

# Run with debug output and SQL logging
sbt -Dslick.ansiDump=true testkit/test

# Cross-version testing
sbt ++2.13.16 testkit/test
sbt ++3.3.4! testkit/test

# Codegen tests (requires cleaning managed sources first)
rm -rf slick-testkit/target/scala-2.13/src_managed && sbt 'testOnly slick.test.codegen.*'
```

### Development Build Tasks

```bash
# Check for dependency updates
sbt dependencyUpdates

# Build capabilities documentation table
sbt testkit/buildCapabilitiesTable

# Run compatibility report
sbt site/buildCompatReport

# Clean compile-time test artifacts
sbt testkit/cleanCompileTimeTests

# Check current version
sbt "show version"

# Run lint and type checking (for PR validation)
# Note: Check build.sbt for specific lint/typecheck commands
```

## Architecture Overview

### Core Layers

1. **AST Layer** (`slick.ast`): Immutable query representation with a comprehensive type system
2. **Compiler Layer** (`slick.compiler`): Multi-phase query compilation pipeline
3. **Lifted Embedding** (`slick.lifted`): Type-safe DSL for database queries
4. **Profile System**: Database-specific implementations (H2Profile, PostgresProfile, etc.)
5. **Backend/JDBC** (`slick.jdbc`): SQL generation and database execution

### Key Components

- **QueryCompiler**: Immutable, configurable compilation pipeline with 20+ phases
- **Profile**: Database abstraction layer with capability-based feature detection
- **DBIOAction**: Monadic database operations with async support and effect tracking
- **Table/Query**: Core abstractions for database schema and queries
- **Rep[T]**: Lifted representation of database values with type safety
- **Shape System**: Type-level mapping between Scala types and database representations

### Database Support

Slick supports multiple databases through profile-based architecture:

- **Production databases**: PostgreSQL, MySQL, SQL Server, Oracle, DB2
- **Embedded databases**: H2, Derby, HSQLDB, SQLite
- **In-memory testing**: Heap-based database for unit tests
- **Profile hierarchy**: BasicProfile → RelationalProfile → SqlProfile → JdbcProfile → Specific profiles

### Custom Logging Support (nafg/custom-logging branch)

- **LoggingContext**: Key-value context information that can be attached to database operations
- **Context Propagation**: Logging context is threaded through the entire execution stack from DBIOAction to SQL logging
- **Enhanced SQL Logging**: SQL statements are logged with associated context information for better traceability
- **Convenience APIs**: Simple methods to attach context to database actions via `withLoggingContext()` and `tagged()` methods

## Code Organization

### Main Source Structure

```
slick/src/main/scala/slick/
├── ast/           # Abstract syntax tree and comprehensive type system
├── compiler/      # Multi-phase query compilation pipeline (20+ phases)
├── lifted/        # Type-safe query DSL with Rep[T] and Query[E,U,C]
├── jdbc/          # JDBC backend and database-specific profiles
├── basic/         # Basic profile abstraction and backend
├── relational/    # Relational database abstractions
├── sql/           # SQL-specific functionality and capabilities
├── memory/        # In-memory heap database support
├── dbio/          # Database I/O action system with effect tracking
└── util/          # Utilities including tuple support and helpers
```

### Test Structure

```
slick-testkit/src/
├── main/scala/    # Testkit framework, utilities, and test definitions
├── test/scala/    # Profile-specific unit tests and benchmarks
├── codegen/       # Code generation tests
└── doctest/       # Documentation example tests
```

### Critical Scala 3 Compatibility

The codebase includes Scala 3-specific implementations:

- **slick/src/main/scala-3/**: Scala 3-specific code including `ShapedValue.scala`
- **slick/src/main/scala-2.13/**: Scala 2.13-specific implementations
- **Cross-compilation**: Supports Scala 2.12, 2.13, and 3.x with version-specific optimizations

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

# Run specific test with full qualification
sbt "testkit/testOnly slick.test.profile.H2MemTest -- -z com.typesafe.slick.testkit.tests.JoinTest.testJoin[h2mem]"

# Run with debug output
sbt -Dslick.ansiDump=true testkit/test

# Run with SQL debugging
sbt -Dslick.ansiDump=true testkit/test
```

### Cross-Database Testing Configuration

#### Configuration System Overview

Slick's testkit uses a layered configuration system for database testing:

1. **Default Configuration** (`testkit-reference.conf`): Embedded databases only (H2, Derby, HSQLDB, SQLite)
   - Fast local testing with no external dependencies

2. **Base Database Configuration** (`test-dbs/testkit-databases.conf`): All service database connection settings
   - Contains connection details for PostgreSQL, MySQL, Oracle, DB2, SQL Server
   - All service databases remain disabled by default
   - Matches the services defined in GitHub Actions as well as `docker-compose.yml`

3. **GitHub Actions Configuration** (`test-dbs/testkit.github-actions.conf`): CI comprehensive testing
   - Includes the base database config and sets `enabled = true` for all service databases
   - Used for comprehensive cross-database testing in CI

4. **System Property Overrides**: Typesafe Config allows runtime configuration overrides
   - `-D[database].enabled=true` enables specific databases when using base config
   - Much simpler than having to disable everything else

#### Testing Service Databases Locally

Use the base config and enable only what you need:

```bash
# Test only PostgreSQL (start docker container and enable)
docker compose up -d postgres
env SLICK_TESTKIT_CONFIG=test-dbs/testkit-databases.conf sbt \
  -Dpostgres.enabled=true \
  "testkit/testOnly slick.test.profile.PostgresTest"

# Test PostgreSQL + MySQL together
docker compose up -d postgres mysql
env SLICK_TESTKIT_CONFIG=test-dbs/testkit-databases.conf sbt \
  -Dpostgres.enabled=true -Dmysql.enabled=true \
  testkit/test

# Test specific functionality across multiple databases
env SLICK_TESTKIT_CONFIG=test-dbs/testkit-databases.conf sbt \
  -Dpostgres.enabled=true -Dmysql.enabled=true \
  'project testkit' \
  '+testOnly -- -z com.typesafe.slick.testkit.tests.AggregateTest.testGroupBy*'
```

**CI Testing** (all databases enabled):

```bash
# Comprehensive testing (as used in GitHub Actions)
env SLICK_TESTKIT_CONFIG=test-dbs/testkit.github-actions.conf sbt testkit/test
```

**Available Database Flags:**

- `postgres.enabled=true` - Enable PostgreSQL testing
- `mysql.enabled=true` - Enable MySQL testing  
- `db2.enabled=true` - Enable DB2 testing
- `oracle.enabled=true` - Enable Oracle testing
- `sqlserver-sqljdbc.enabled=true` - Enable SQL Server testing

#### Why This Design?

- **Local simplicity**: Enable only what you want to test with `-D[database].enabled=true`
- **Resource efficiency**: No need to disable multiple databases - just enable what you need
- **CI simplicity**: GitHub Actions config enables everything for comprehensive testing
- **Consistent connection settings**: Same connection parameters across all environments
- **Clear separation**: Base config has connection details, CI config just enables everything

### Local Database Testing with Docker

```bash
# Start databases using docker-compose
docker compose up -d postgres
docker compose up -d mysql
docker compose up -d sqlserver

# Wait for database readiness
docker compose exec postgres pg_isready -U postgres
```

### Test Development Patterns

- **TestDB abstraction**: Database-specific test configurations with capability detection
- **ResultSetAction**: Use for custom metadata queries with proper connection context
- **Database cleanup**: Override `dropUserArtifacts` for database-specific cleanup logic
- **CI configuration**: Use `test-dbs/testkit.github-actions.conf` for CI-specific database settings
- **Test concurrency**: Database tests are grouped and run with controlled parallelism

## Build System and Module Structure

### SBT Configuration

- **Multi-module project**: Root project aggregates slick, codegen, hikaricp, testkit, and site modules
- **Cross-compilation**: Supports Scala 2.12, 2.13, and 3.x with version-specific optimizations
- **Custom tasks**: `testAll` runs comprehensive test suite across all modules
- **Dependencies**: Managed in `project/Dependencies.scala` with version centralization
- **Test concurrency**: Configured with test group tags to limit concurrent database tests
- **Forked tests**: Database tests run in forked JVMs for isolation

### Module Dependencies

- **slick**: Core library, no dependencies on other modules
- **slick-testkit**: Depends on slick, codegen, hikaricp for comprehensive testing
- **slick-codegen**: Depends on slick for database schema code generation
- **slick-hikaricp**: Depends on slick for connection pooling integration
- **reactive-streams-tests**: Depends on testkit for streaming compliance tests

### Version Management

The project uses a custom versioning system:

- **Custom Versioning**: `project/Versioning.scala` controls version format (e.g., `3.7.0-pre.32.abc123.dirty`)
- **SBT Integration**: Custom plugin overrides sbt-ci-release to use the custom versioning scheme
- **Version Policy**: Uses sbt-version-policy for compatibility checking across versions
- **Pattern Matching**: Internal dependency version changes are filtered using regex patterns in `versionPolicy.sbt`

## Compatibility Checking System

### CompatReportPlugin

Slick uses a custom `CompatReportPlugin` that integrates with sbt-version-policy and MiMa for binary compatibility
checking:

- **Binary Compatibility**: Uses MiMa to detect breaking changes in public APIs
- **Source Compatibility**: Tracks changes that break source compatibility
- **Dependency Tracking**: Monitors internal module dependencies for version consistency
- **GitHub Integration**: Automatically posts compatibility reports on PRs via GitHub Actions

### Version Policy Configuration

- **Internal Dependencies**: Filtered using `versionPolicyIgnoredInternalDependencyVersions` to avoid noise
- **MiMa Filters**: Exclusions defined in `slick/src/main/mima-filters/` for legitimate breaking changes
- **Compatibility Reports**: Generated via `sbt site/buildCompatReport` and posted to PRs automatically

### Common Compatibility Commands

```bash
# Generate compatibility report manually
sbt site/buildCompatReport

# Check version policy compliance
sbt versionPolicyCheck

# Test with specific previous version
sbt 'set every CompatReportPlugin.previousRelease := Some("3.6.1-pre.123.abc")' site/buildCompatReport

# Show current version policy settings
sbt "show versionPolicyIgnored"
sbt "show versionPolicyIgnoredInternalDependencyVersions"
```

## Compiler Pipeline Architecture

### Query Compilation Phases

The `QueryCompiler` uses an immutable, configurable pipeline:

1. **AssignUniqueSymbols**: Symbol resolution and uniqueness
2. **InferTypes**: Type inference and validation
3. **ExpandTables**: Table expansion and normalization
4. **RewriteJoins**: Join transformation and optimization
5. **MergeToComprehensions**: Query optimization and flattening
6. **ResolveZipJoins**: Zip join resolution
7. **Database-specific phases**: SQL generation for the target database

### Compilation Customization

- **Profile-specific compilers**: Each database profile has customized compilation
- **Phase modification**: Phases can be added, removed, or replaced
- **Debug support**: Compilation logging and AST dumping capabilities
- **Type system integration**: Comprehensive type annotations throughout compilation

## Advanced Development Patterns

### AST Manipulation

- **Immutable nodes**: All AST nodes are immutable with structural sharing
- **Type system**: Comprehensive type annotations with `TypedType[T]` throughout the AST
- **Node transformation**: Pattern matching and tree rewriting for optimizations
- **Symbol management**: Unique symbol generation and resolution

### Database Profile System

- **Capability-based**: Each profile declares supported database features
- **SQL generation**: Profile-specific SQL builders and formatters
- **Type mapping**: Database-specific type conversions and encodings
- **Feature detection**: Runtime capability checking for database features

### Performance Considerations

- **Streaming support**: Reactive Streams integration for large result sets
- **Connection pooling**: HikariCP integration with monitoring support
- **Async operations**: Future-based API with proper resource management
- **Compilation caching**: Prepared statement caching and reuse

## Development Guidelines

### Code Style

- Follow Scala best practices and existing codebase patterns
- Leverage the type system for compile-time safety
- Write comprehensive tests for new features
- Use immutable data structures throughout

### Database Compatibility

- New features should work across supported databases
- Use capability system to handle database-specific features
- Test against multiple database profiles
- Handle database-specific SQL generation in profiles
- **Aggregate Functions**: When modifying aggregate functions (like AVG), be aware that different databases return
  different types (INTEGER vs DECIMAL vs DOUBLE). Consider using `Option[Double]` for consistency and add
  database-specific rounding logic in test suites

### Performance Considerations

- Compiler phases are optimized for query performance
- Use streaming for large result sets via `DatabasePublisher`
- Async operations with proper resource management
- Connection pooling with HikariCP monitoring

## Common Issues and Solutions

### Compilation Problems

- **Type inference issues**: Check `slick.ast.Type` system and `TypedType` implicits
- **Implicit resolution**: Review `Shape` and `TypedType` implicits in `slick.lifted`
- **Compiler phases**: Debug with `QueryCompiler` logging and AST dump
- **Scala 3 compatibility**: Check version-specific source directories

### Database Testing Issues

- **Connection issues**: Check database configuration in `application.conf`
- **SQL generation**: Enable SQL logging with `slick.ansiDump=true`
- **Database-specific features**: Verify profile capabilities
- **PostgreSQL JDBC Compatibility**: When upgrading PostgreSQL JDBC drivers, catalog parameter handling may change. Use
  `conn.getCatalog` instead of empty strings in `DatabaseMetaData.getTables()` calls
- **"Relation already exists" errors**: Usually indicates database cleanup issues between tests. Check
  `dropUserArtifacts` implementations in test database configurations

### Code Generation (Codegen) Issues

- **H2 VARCHAR Length Problems**: H2 reports default VARCHAR length as 1000000000, which can cause issues with other
  databases during round-trip testing. This is handled in `H2ColumnBuilder.length` by filtering out large default values
- **Derby VARCHAR Limits**: Derby has a VARCHAR length limit of 32,672 characters. Codegen tests use H2 for generation
  but Derby for execution, requiring careful length handling
- **Cross-Database DDL Compatibility**: When generating code from one database profile for use with another, be aware of
  capability differences (e.g., H2's features vs Derby's limitations)
- **Codegen Test Cleanup**: Always delete `slick-testkit/target/scala-2.13/src_managed` before running codegen tests to
  ensure clean regeneration

### Performance Issues

- **Large queries**: Use streaming with `DatabasePublisher`
- **Connection pooling**: Configure HikariCP appropriately
- **Query optimization**: Review compiled SQL output with debug logging

### Scala 3 Compatibility Issues

- **Type inference changes**: Scala 3 has different type inference behavior
- **Match types**: Use Scala 3's match types for type-level programming
- **Anonymous class handling**: Be aware of changes in anonymous class generation
- **Coverage instrumentation**: Some Scala 3 versions have issues with coverage tools

## Key Files to Understand

### Core Architecture

- `slick/ast/Node.scala`: Base AST node with comprehensive type system
- `slick/compiler/QueryCompiler.scala`: Multi-phase compilation pipeline
- `slick/lifted/Query.scala`: Main query interface and combinators
- `slick/lifted/Rep.scala`: Lifted representation with type safety
- `slick/jdbc/JdbcProfile.scala`: JDBC backend implementation

### Database Profiles

- `slick/jdbc/H2Profile.scala`: H2 database support (default for testing)
- `slick/jdbc/PostgresProfile.scala`: PostgreSQL support
- `slick/basic/BasicProfile.scala`: Base profile abstraction

### Testing Infrastructure

- `slick-testkit/src/main/scala/com/typesafe/slick/testkit/util/TestDB.scala`: Test database management
- `slick-testkit/src/main/scala/com/typesafe/slick/testkit/util/StandardTestDBs.scala`: Standard test configurations
- `test-dbs/testkit.github-actions.conf`: CI-specific database configurations
- `docker-compose.yml`: Local database setup for testing

### Build and Version Management

- `project/Versioning.scala`: Custom versioning logic with sbt-ci-release integration
- `project/CompatReportPlugin.scala`: Custom compatibility checking plugin
- `versionPolicy.sbt`: Version policy configuration and internal dependency filtering
- `project/Dependencies.scala`: Centralized dependency management with version coordination

### Scala 3 Compatibility

- `slick/src/main/scala-3/slick/lifted/ShapedValue.scala`: Scala 3-specific implementations
- `slick/src/main/scala/slick/lifted/Rep.scala`: Core representation types

### Custom Logging Infrastructure (nafg/custom-logging branch)

- `slick/util/LoggingContext.scala`: Context information that can be propagated through database operations
- `slick/util/Logging.scala`: Enhanced SlickLogger with context support in log messages
- `slick/basic/BasicBackend.scala`: Backend support for threading logging context through operations
- `slick/jdbc/JdbcBackend.scala`: JDBC-specific context propagation and statement creation
- `slick/jdbc/LoggingStatement.scala`: Statement wrapper that includes context in SQL logs
- `slick/dbio/DBIOAction.scala`: Convenience methods for attaching context to database actions

## Critical Development Notes

### Database Driver Compatibility

- **JDBC driver upgrades**: Always test against multiple database versions when updating drivers
- **Metadata API changes**: DatabaseMetaData behavior can change between driver versions, especially catalog parameter
  handling
- **Test isolation**: Database cleanup between tests relies on proper metadata queries

### Scala Version Compatibility

- **Cross-compilation**: Test thoroughly across Scala 2.12, 2.13, and 3.x
- **Version-specific code**: Use `scala-2.13/` and `scala-3/` source directories appropriately
- **Type inference**: Be aware of Scala 3's different type inference behavior
- **Anonymous classes**: Changes in anonymous class generation can affect type casting

### Testing Best Practices

- **Use H2 for fast local testing**: Default database for rapid development cycles
- **Test with real databases**: Use PostgreSQL/MySQL for production-like testing
- **Database cleanup**: Ensure proper cleanup between tests to avoid "relation exists" errors
- **Test grouping**: Respect database-specific test groups for concurrency control

### Version Management Notes

- **Custom versioning precedence**: The `Versioning` plugin requires `CiReleasePlugin` to ensure proper load order
- **Version format**: Follows pattern `X.Y.Z-pre.N.SHA[.dirty]` for development builds
- **Compatibility filtering**: Internal dependency changes are filtered using regex patterns to avoid CI noise
- **MiMa integration**: Binary compatibility is checked using MiMa with custom exclusion filters

### Custom Logging Development Notes

- **Context Threading**: LoggingContext must be properly threaded through all backend operations for consistent logging
- **Backward Compatibility**: All new logging APIs maintain backward compatibility with existing code
- **Performance**: LoggingContext operations are designed to be lightweight with minimal allocation overhead
- **Testing**: Custom logging features should be tested across all supported database profiles
- **Usage Pattern**: Attach context early in the operation chain using `action.withLoggingContext()` or `action.tagged()`
