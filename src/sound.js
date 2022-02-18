//audioを作成
var audio01 = new Audio();
var audio02 = new Audio();
var audio11 = new Audio();
var audio12 = new Audio();
var audio21 = new Audio();
var audio22 = new Audio();
var audio30 = new Audio();
var audio40 = new Audio();

//プリロード
audio01.preload = "auto";
audio02.preload = "auto";
audio11.preload = "auto";
audio12.preload = "auto";
audio21.preload = "auto";
audio22.preload = "auto";
audio30.preload = "auto";
audio40.preload = "auto";
//音のバスを指定
audio01.src = "./music01.wav";
audio02.src = "./music02.wav";
audio11.src = "./music11.wav";
audio12.src = "./music12.wav";
audio21.src = "./music21.wav";
audio22.src = "./music22.wav";
audio30.src = "./step.wav";
audio40.src = "./get.wav";
//読み込む
audio01.load();
audio02.load();
audio11.load();
audio12.load();
audio21.load();
audio22.load();
audio30.load();
audio40.load();
audio02.volume = 0;
audio12.volume = 0;

function playAudio() {
    audio02.volume = 0;
    audio01.play();
    audio02.play();
}

function playAudio1() {
    audio12.volume = 0;
    audio11.play();
    audio12.play();
}

function resultAudio() {
    stopAudio();
    audio21.play();
}

function stopAudio() {
    audio01.pause();
    audio02.pause();
    audio11.pause();
    audio12.pause();
    audio01.load();
    audio02.load();
    audio11.load();
    audio12.load();
}

function stepAudio() {
    audio30.load();
    audio30.play();
}
function getAudio() {
    audio40.load();
    audio40.play();
}

audio01.addEventListener("ended", function() {
    audio01.currentTime = 0;
    audio02.currentTime = 0;
    audio01.play();
    audio02.play();
}, false);

audio11.addEventListener("ended", function() {
    audio11.currentTime = 0;
    audio12.currentTime = 0;
    audio11.play();
    audio12.play();
}, false);

audio21.addEventListener("ended", function() {
    audio21.pause();
    audio21.load();
    audio22.play();
}, false);

audio22.addEventListener("ended", function() {
    audio22.currentTime = 0;
    audio22.play();
}, false);

function stopAudio2() {
    audio21.pause();
    audio22.pause();
    audio21.load();
    audio22.load();
}