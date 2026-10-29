# AGENTS.md

This file provides guidance to coding agents working with code in this repository.

## Project Overview

Slick is a functional relational mapping (FRM) library for Scala that provides type-safe, composable database access.
It's a multi-module sbt project; `build.sbt` defines the modules.

## Common Development Commands

### Building and Compilation

```bash
# Clean and compile all modules. The sbt 2 runner joins all arguments into one command line,
# so separate multiple commands with ';' inside a single quoted argument.
sbt "clean; compile"

# Compile under each supported Scala version. `++2.13.x` switches every project to the 2.13 version listed in
# `crossScalaVersions`
sbt ++2.12.x compile
sbt ++2.13.x compile
sbt ++3.x compile

# Generate API documentation
sbt doc
```

### Running Tests

Read [DEVELOPING.md](DEVELOPING.md) before running tests. It covers the setting needed to load the build in a linked
git worktree, how to select testkit tests (`testOnly` patterns and the arguments after `--`), how to set system
properties in the forked test JVMs, and how to enable the service databases.

### Development Build Tasks

```bash
# Build capabilities documentation table
sbt testkit/buildCapabilitiesTable

# Run compatibility report
sbt site/buildCompatReport

# Check current version
sbt "show version"
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

## Testing Framework

### Test Configuration System

Tests use Typesafe Config for database configuration:

- **testkit-reference.conf**: Defines test database configurations and test classes
- **Multi-database support**: PostgreSQL, MySQL, Oracle, DB2, SQL Server, H2, Derby, HSQLDB, SQLite
- **Test grouping**: Database-specific test groups with controlled concurrency
- **Environment-specific configs**: GitHub Actions configurations in `test-dbs/`
- **Key test databases**: H2 (default for fast testing), PostgreSQL, MySQL (for production database testing)

### Test Development Patterns

- **TestDB abstraction**: Database-specific test configurations with capability detection
- **ResultSetAction**: Use for custom metadata queries with proper connection context
- **Database cleanup**: Override `dropUserArtifacts` for database-specific cleanup logic
- **CI configuration**: Use `test-dbs/testkit.github-actions.conf` for CI-specific database settings
- **Test concurrency**: Database tests are grouped and run with controlled parallelism

## Build System and Module Structure

### SBT Configuration

- **Cross-compilation**: Supports Scala 2.12, 2.13, and 3.x with version-specific optimizations
- **Custom tasks**: `testAll` runs comprehensive test suite across all modules
- **Dependencies**: Managed in `project/Dependencies.scala` with version centralization
- **Test concurrency**: Configured with test group tags to limit concurrent database tests
- **Forked tests**: Database tests run in forked JVMs for isolation

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
sbt 'set every CompatReportPlugin.previousRelease := Some("3.6.1-pre.123.abc"); site/buildCompatReport'

# Show current version policy settings
sbt "show versionPolicyIgnored"
sbt "show versionPolicyIgnoredInternalDependencyVersions"
```

## Compiler Pipeline Architecture

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

## Common Issues and Solutions

### Compilation Problems

- **Type inference issues**: Check `slick.ast.Type` system and `TypedType` implicits
- **Implicit resolution**: Review `Shape` and `TypedType` implicits in `slick.lifted`
- **Compiler phases**: Debug with `QueryCompiler` logging and AST dump
- **Scala 3 compatibility**: Check version-specific source directories

### Database Testing Issues

- **SQL generation**: Log the executed SQL with `-Dlog.jdbc.statement=debug` (see "Setting system properties for
  tests" in DEVELOPING.md)
- **Database-specific features**: Verify profile capabilities
- **"Relation already exists" errors**: Usually indicates database cleanup issues between tests. Check
  `dropUserArtifacts` implementations in test database configurations

### Code Generation (Codegen) Issues

- **Cross-Database DDL Compatibility**: When generating code from one database profile for use with another, be aware of
  capability differences (e.g., H2's features vs Derby's limitations)

### Scala 3 Compatibility Issues

- **Type inference changes**: Scala 3 has different type inference behavior
- **Match types**: Use Scala 3's match types for type-level programming
- **Anonymous class handling**: Be aware of changes in anonymous class generation
- **Coverage instrumentation**: Some Scala 3 versions have issues with coverage tools

## Key Files to Understand

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

## Critical Development Notes

### Database Driver Compatibility

- **JDBC driver upgrades**: Always test against multiple database versions when updating drivers
- **Metadata API changes**: DatabaseMetaData behavior can change between driver versions, especially catalog parameter
  handling
- **Test isolation**: Database cleanup between tests relies on proper metadata queries

### Scala Version Compatibility

- **Cross-compilation**: Test thoroughly across Scala 2.12, 2.13, and 3.x
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
