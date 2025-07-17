# Copilot Instructions for Slick Project

## Project Overview
This is Slick, a functional relational mapping (FRM) library for Scala that provides type-safe, composable database access. It's a multi-module SBT project with comprehensive testing across multiple database platforms.

## Key Development Guidelines

### Code Style and Conventions
- Follow existing Scala code style and patterns found in the codebase
- Do not add comments unless explicitly requested
- Use immutable data structures throughout
- Leverage the type system for compile-time safety
- Follow functional programming principles

### Build and Testing
- Use `sbt` for all build operations
- Main build command: `sbt clean compile`
- Test command: `sbt testAll` (includes testkit, doctests, and reactive-streams tests)
- Cross-compile testing: `sbt ++2.12.20 compile`, `sbt ++2.13.16 compile`, `sbt ++3.3.4 compile`
- For database-specific testing, use the testkit with appropriate configuration

### Database Support
- The project supports multiple databases: PostgreSQL, MySQL, SQL Server, Oracle, DB2, H2, Derby, HSQLDB, SQLite
- Use H2 for fast local testing (default)
- Use PostgreSQL/MySQL for production-like testing
- Database configurations are in `test-dbs/` directory
- All database tests use the testkit framework

### Architecture Knowledge
- Core layers: AST, Compiler, Lifted Embedding, Profile System, Backend/JDBC
- Query compilation uses a multi-phase pipeline (20+ phases)
- Database abstraction through profile-based architecture
- Type-safe DSL using Rep[T] and Query[E,U,C]

### Testing Patterns
- Use the testkit framework for database tests
- Tests are organized by database profile and functionality
- Respect test concurrency controls for database tests
- Clean up database artifacts between tests

### Compatibility Requirements
- Maintain binary compatibility using MiMa
- Support Scala 2.12, 2.13, and 3.x
- Use version-specific source directories when needed
- Test across supported database versions

### Common Commands
- `sbt testkit/test` - Run testkit tests only
- `sbt "testkit/testOnly *JoinTest*"` - Run specific test patterns
- `sbt -Dslick.ansiDump=true testkit/test` - Run with debug output
- `sbt dependencyUpdates` - Check for dependency updates
- `sbt site/buildCompatReport` - Generate compatibility report

### Important Files to Reference
- `CLAUDE.md` - Comprehensive project documentation and commands
- `build.sbt` - Main build configuration
- `project/Dependencies.scala` - Dependency management
- `slick-testkit/` - Test framework and database tests
- `test-dbs/` - Database configuration files

### Security and Best Practices
- Never commit secrets or keys to the repository
- Follow security best practices in code changes
- Maintain existing test coverage when making changes
- Use proper error handling and resource management

## When Making Changes
1. Read existing code to understand patterns and conventions
2. Check imports and dependencies to understand framework choices
3. Run relevant tests to ensure changes work correctly
4. Follow the existing architectural patterns
5. Maintain type safety throughout the codebase
6. Test against multiple Scala versions when relevant