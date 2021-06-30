import {Elm} from "./Main.elm";

const element = document.getElementById("app");
if (element) {
    const app = Elm.Main.init({node: element, flags: null});

}