pub struct BeepSound {
    sink: rodio::Sink,
    // Needs to be kept alive, since stream is destroyed when this object is dropped.
    _stream: rodio::OutputStream,
}

impl BeepSound {
    pub fn new() -> Self {
        let mut stream = rodio::OutputStreamBuilder::open_default_stream().unwrap();
        stream.log_on_drop(false);

        let sink = rodio::Sink::connect_new(stream.mixer());
        let wave = rodio::source::SineWave::new(440.0);
        sink.append(wave);

        sink.set_volume(0.1);
        sink.pause();

        Self {
            sink,
            _stream: stream,
        }
    }

    pub fn on(&self) {
        self.sink.play();
    }

    pub fn off(&self) {
        self.sink.pause();
    }
}
