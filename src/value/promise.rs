use crate::{Value, error::RuntimeError, value::IntoPetitResult};
use std::{
    fmt::{self, Debug, Display},
    pin::Pin,
    sync::Arc,
};
use tokio::sync::Mutex;

/// TODO
#[derive(Clone)]
pub struct Promise {
    inner: Arc<Mutex<PromiseInner>>,
}

impl Promise {
    /// TODO
    pub fn new(
        // TODO this needs to take a process (or similar) to map the trace
        future: impl 'static + Future<Output = impl IntoPetitResult> + Send + Sync,
    ) -> Self {
        // Wrap the future to map the output result
        let future = Box::pin(async move {
            future
                .await
                .into_petit_result()
                .map_err(|error| RuntimeError {
                    error,
                    trace: todo!(),
                    source_table: todo!(),
                })
        });
        Self {
            inner: Arc::new(Mutex::new(PromiseInner::Future(future))),
        }
    }

    /// TODO
    pub async fn resolve(self) -> Result<Value, RuntimeError> {
        // If the promise is being resolved elsewhere, we'll wait here until
        // it is.
        let mut inner = self.inner.lock().await;
        match &mut *inner {
            PromiseInner::Future(future) => {
                // TODO what happens if resolve() is called again on a future
                // that failed? I think that violates the Future trait's req
                // that poll is never called after the future is done.
                let value = future.await?;
                *inner = PromiseInner::Complete(value.clone());
                Ok(value)
            }
            PromiseInner::Complete(value) => Ok(value.clone()),
        }
    }
}

impl Debug for Promise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Display for Promise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl PartialEq for Promise {
    fn eq(&self, other: &Self) -> bool {
        // Promises are equal if the arc points to the same inner future
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

/// TODO
enum PromiseInner {
    /// TODO
    Future(PromiseFuture),
    /// TODO
    Complete(Value),
}

/// TODO
pub type PromiseFuture = Pin<
    Box<
        dyn 'static
            + Future<Output = Result<Value, RuntimeError>>
            + Send
            + Sync,
    >,
>;

/*
TODO delete notes
async function command(cmd: str): Promise<str>;

// Does this kick off a future? when does it get polled?
command('run')

registerPromise(command('run'));

// Rust:
fn register_promise(process:&mut Process, promise:Promise) {
    let future = process.promises.get(promise.id);
    process.promises.insert(promise);
}

asyncness has to either be propagated up through the eval stack or handled
at the call site (in native fns)
- up through call stack - all eval/exec fns have to become async
- on-site - pushed into a background worker and block on
*/
