/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [],
  content: ["./src/**/*.hs"],
  theme: {
    extend: {
      colors: {
        'chili_red': { DEFAULT: '#db230b', 100: '#2c0702', 200: '#570e05', 300: '#831507', 400: '#ae1c09', 500: '#db230b', 600: '#f4412a', 700: '#f7715f', 800: '#f9a094', 900: '#fcd0ca' },
        'night': { DEFAULT: '#0a0a08', 100: '#020202', 200: '#050504', 300: '#070705', 400: '#090907', 500: '#0a0a08', 600: '#424235', 700: '#787860', 800: '#a8a892', 900: '#d3d3c9' },
        'chili_red': { DEFAULT: '#e61903', 100: '#2e0501', 200: '#5c0b01', 300: '#8b1002', 400: '#b91503', 500: '#e61903', 600: '#fc3b26', 700: '#fd6c5c', 800: '#fd9d92', 900: '#fecec9' },
        'jet': { DEFAULT: '#2c2721', 100: '#090807', 200: '#11100d', 300: '#1a1714', 400: '#231f1a', 500: '#2c2721', 600: '#5d5346', 700: '#8e7f6b', 800: '#b4aa9c', 900: '#dad4ce' },
        'penn_red': { DEFAULT: '#9b1708', 100: '#1f0502', 200: '#3e0903', 300: '#5d0e05', 400: '#7c1207', 500: '#9b1708', 600: '#dd210c', 700: '#f54c39', 800: '#f8887b', 900: '#fcc3bd' },
      },
    },
  },
  plugins: [],
}

