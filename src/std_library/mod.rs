mod targets;

#[cfg(feature = "unix-std_library")]
mod unix;

#[cfg(feature = "voxeon_os-std_library")]
mod voxeon_os;
