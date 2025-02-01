/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [],
  content: ["./src/**/*.hs"],
  theme: {
    extend: {
      colors: {
        blue: {
          500: '#60A5FA',
          600: '#3B82F6',
        },
        gray: {
          50: '#F9FAFB',
          200: '#E5E7EB',
          700: '#374151',
          800: '#1F2937',
        },
      },
    },
  },
  plugins: [],
}

