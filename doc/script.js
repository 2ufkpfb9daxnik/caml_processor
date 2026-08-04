const themeToggle = document.querySelector('.theme-toggle');
const themeKey = 'ocaml-introduction-theme';

const setTheme = (theme) => {
    document.body.classList.toggle('theme-dark', theme === 'dark');
    themeToggle.textContent = '☀️/🌙';
    try {
        localStorage.setItem(themeKey, theme);
    } catch (_) {
    }
};

let savedTheme = 'light';
try {
    savedTheme = localStorage.getItem(themeKey) || (matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
} catch (_) {
    savedTheme = matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

setTheme(savedTheme);
themeToggle.addEventListener('click', () => {
    setTheme(document.body.classList.contains('theme-dark') ? 'light' : 'dark');
});