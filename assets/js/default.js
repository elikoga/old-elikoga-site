$(function () {
    $('[data-toggle="tooltip"]').tooltip();
});

function copyString(string) {
    navigator.clipboard.writeText(string);
}