use std::{future::Future, pin::Pin};

pub type BoxFuture<'a, Output> = Pin<Box<dyn 'a + Future<Output = Output>>>;

/// Extension trait for futures. This reimplements some stuff from the `futures`
/// crate, so we don't have to pull the full thing in
pub trait FutureExt: Future {
    /// Convenience function for boxing a future in a method chain
    fn boxed<'a>(self) -> BoxFuture<'a, Self::Output>
    where
        Self: 'a;
}

impl<F> FutureExt for F
where
    F: Future,
{
    fn boxed<'a>(self) -> BoxFuture<'a, Self::Output>
    where
        Self: 'a,
    {
        Box::pin(self)
    }
}
