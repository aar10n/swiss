use crate::print::DisplayString;
use crate::runtime::{Context, Exception, FileHandle, Handle, UserTy, Value};
use std::env;
use std::fs;
use std::path::{Component, Path, PathBuf};

fn wrap_file(file: FileHandle) -> Value {
    Value::UserType(UserTy::Handle(Handle::new("file".into(), file)))
}

pub(super) fn register(ctx: &mut Context) {
    // file handle methods
    ctx.module_mut("builtin")
        .expect("builtin module should exist")
        .new_submodule("file_impl")
        .unwrap()
        .with_function(builtin_fn_v2!("close", |&ctx, f: file| {
            f.close();
            Ok(Value::default())
        }))
        .with_function(builtin_fn_v2!("is_open", |&ctx, f: file| {
            Ok(Value::Boolean(f.is_open()))
        }))
        .with_function(builtin_fn_v2!("read_all", |&ctx, f: file| {
            f.read_all().map(Value::String).map_err(|e| {
                Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
            })
        }))
        .with_function(builtin_fn_v2!("write", |&ctx, f: file, v: any| {
            let content = match &v {
                Value::String(s) => s.clone(),
                _ => v.display_string(ctx),
            };
            f.write_str(&content)
                .map(|_| Value::default())
                .map_err(|e| {
                    Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
                })
        }))
        .with_function(builtin_fn_v2!("writeln", |&ctx, f: file, v: any| {
            let content = match &v {
                Value::String(s) => s.clone(),
                _ => v.display_string(ctx),
            };
            f.write_line(&content)
                .map(|_| Value::default())
                .map_err(|e| {
                    Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
                })
        }))
        .with_function(builtin_fn_v2!("flush", |&ctx, f: file| {
            f.flush().map(|_| Value::default()).map_err(|e| {
                Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
            })
        }));

    // fs submodule
    ctx
        .module_mut("builtin")
        .expect("builtin module should exist")
        .new_submodule("fs")
        .unwrap()
        .with_function(builtin_fn_v2!("open", |&ctx, path: str, mode: str?| {
            let mode = mode.unwrap_or_else(|| "r".to_string());
            let result = match mode.as_str() {
                "r" => FileHandle::open_read(&path),
                "w" => FileHandle::open_write(&path),
                "a" => FileHandle::open_append(&path),
                "r+" => FileHandle::open_read_write(&path),
                _ => {
                    return Err(Exception::new(
                        "ValueError",
                        format!("invalid file mode '{}'", mode),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            };
            match result {
                Ok(file) => Ok(wrap_file(file)),
                Err(e) => Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())),
            }
        }))
        .with_function(builtin_fn_v2!("open_write", |&ctx, path: str| {
            match FileHandle::open_write(&path) {
                Ok(file) => Ok(wrap_file(file)),
                Err(e) => {
                    Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
                }
            }
        }))
        .with_function(builtin_fn_v2!("open_append", |&ctx, path: str| {
            match FileHandle::open_append(&path) {
                Ok(file) => Ok(wrap_file(file)),
                Err(e) => {
                    Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
                }
            }
        }))
        .with_function(builtin_fn_v2!("open_read_write", |&ctx, path: str| {
            match FileHandle::open_read_write(&path) {
                Ok(file) => Ok(wrap_file(file)),
                Err(e) => {
                    Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
                }
            }
        }))
        .with_function(builtin_fn_v2!("listdir", |&ctx, path: str?| {
            let path = match path {
                Some(path) => PathBuf::from(path),
                None => env::current_dir()
                    .map_err(|e| Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))?,
            };

            let entries = fs::read_dir(&path)
                .map_err(|e| Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))?;
            let mut values = Vec::new();
            for entry in entries {
                let entry = entry.map_err(|e| {
                    Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
                })?;
                let name = entry.file_name().to_string_lossy().into_owned();
                values.push(Value::String(name));
            }
            Ok(Value::list(values))
        }))
        .with_function(builtin_fn_v2!("cwd", |&ctx| {
            let cwd = env::current_dir().map_err(|e| {
                Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
            })?;
            Ok(cwd.to_string_lossy().into_owned())
        }))
        .with_function(builtin_fn_v2!("exists", |&ctx, path: str| {
            Ok(Path::new(&path).exists())
        }))
        .with_function(builtin_fn_v2!("mkdir", |&ctx, path: str, recursive: bool?| {
            let path_ref = Path::new(&path);
            if path_ref.exists() {
                if path_ref.is_dir() {
                    return Ok(Value::Boolean(false));
                }
                return Err(
                    Exception::new("IoError", "path exists".into()).with_backtrace(ctx.backtrace()),
                );
            }

            let recursive = recursive.unwrap_or(false);
            let result = if recursive {
                fs::create_dir_all(path_ref)
            } else {
                fs::create_dir(path_ref)
            };

            result
                .map(|_| Value::Boolean(true))
                .map_err(|e| Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
        }))
        .with_function(builtin_fn_v2!("remove", |&ctx, path: str| {
            let path_ref = Path::new(&path);
            let result = if path_ref.is_dir() {
                fs::remove_dir(path_ref)
            } else {
                fs::remove_file(path_ref)
            };
            result
                .map(|_| Value::default())
                .map_err(|e| Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
        }));

    // path submodule
    ctx.module_mut("builtin")
        .expect("builtin module should exist")
        .new_submodule("path")
        .unwrap()
        .with_function(builtin_fn_v2!("join", |&ctx, a: str, b: str| {
            let joined = PathBuf::from(&a).join(&b);
            Ok(joined.to_string_lossy().into_owned())
        }))
        .with_function(builtin_fn_v2!("dirname", |&ctx, path: str| {
            let parent = Path::new(&path)
                .parent()
                .map(|p| p.to_string_lossy().into_owned())
                .unwrap_or_else(|| ".".to_string());
            Ok(parent)
        }))
        .with_function(builtin_fn_v2!("basename", |&ctx, path: str| {
            let base = Path::new(&path)
                .file_name()
                .map(|p| p.to_string_lossy().into_owned())
                .unwrap_or_default();
            Ok(base)
        }))
        .with_function(builtin_fn_v2!("extname", |&ctx, path: str| {
            Ok(Path::new(&path)
                .extension()
                .map(|ext| ext.to_string_lossy().into_owned())
                .unwrap_or_default())
        }))
        .with_function(builtin_fn_v2!("normalize", |&ctx, path: str| {
            let mut parts: Vec<String> = Vec::new();
            let mut absolute = false;
            let mut prefix: Option<String> = None;

            for comp in Path::new(&path).components() {
                match comp {
                    Component::Prefix(p) => {
                        prefix = Some(p.as_os_str().to_string_lossy().into_owned());
                    }
                    Component::RootDir => {
                        absolute = true;
                    }
                    Component::CurDir => {}
                    Component::ParentDir => {
                        if let Some(last) = parts.pop() {
                            if last == ".." {
                                parts.push(last);
                                parts.push("..".to_string());
                            }
                        } else if !absolute {
                            parts.push("..".to_string());
                        }
                    }
                    Component::Normal(s) => {
                        parts.push(s.to_string_lossy().into_owned());
                    }
                }
            }

            let mut normalized = PathBuf::new();
            if let Some(prefix) = prefix {
                normalized.push(prefix);
            }
            if absolute {
                normalized.push(Path::new("/"));
            }
            for part in parts {
                normalized.push(part);
            }

            Ok(normalized.to_string_lossy().into_owned())
        }))
        .with_function(builtin_fn_v2!("is_abs", |&ctx, path: str| {
            Ok(Path::new(&path).is_absolute())
        }));
}
