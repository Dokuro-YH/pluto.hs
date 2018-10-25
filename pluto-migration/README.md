# Pluto-Migration
一个数据库Migration工具

## 安装方式
```
stack install pluto-migration
```

## 使用方式
```
pluto-migration [COMMAND] ... [OPTIONS]

Common flags:
  -d --database_url=DATABASE_URL
     --dir=DIR
  -h --help                       Display help message
  -V --version                    Print version information
     --numeric-version            Print just the version number

pluto-migration setup [OPTIONS]
  Creates the migrations dirctory, creates the database specified in your
  DATABASE_URL, and then runs any existing migrations.

pluto-migration generate [OPTIONS] NAME
  Generate a new migration with the given name, and the current timestamp as
  the version.

pluto-migration pending [OPTIONS]
  Returns true if there are any pending migrations.

pluto-migration install [OPTIONS]
  Install all pending migrations.

pluto-migration revert [OPTIONS]
  Revert all installed migrations.

pluto-migration redo [OPTIONS]
  Redo all installed migrations.
```
