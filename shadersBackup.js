/**
 * All shaders combined in a single file for simplicity
 */
window.shaders = {
  // Vertex shader (used by all effects)
  vertex: `
    varying vec2 vUv;
    void main() {
        vUv = uv;
        gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  
  // Main visualizer shaders
  pulse: `
    uniform float time;
    uniform float timeX;
    uniform vec2 resolution;
    uniform float bass;
    uniform float mid;
    uniform float treble;
    uniform float total;
    uniform float bassDrive;
    uniform float midDrive;
    uniform float trebleDrive;
    uniform float amp;
    uniform float zoom;
    
    varying vec2 vUv;

    vec3 palette(float t) {
      vec3 a = vec3(0.5, 0.5, 0.5);
      vec3 b = vec3(0.5, 0.5, 0.5);
      vec3 c = vec3(1.0, 1.0, 1.0);
      vec3 d = vec3(0.263, 0.416, 0.557);
      return a + b * cos(6.28318 * (c * t + d));
    }
    
    mat2 rotate2D(float angle) {
      float s = sin(angle *1.618), c = cos(angle * 1.618);
      return mat2(c, -s, s, c);
    }
  
    void main() {
      float t = time * 0.02;
      float xfac = total * 0.005;
      
      vec2 uv = (gl_FragCoord.xy * 2.0 - resolution.xy) / resolution.y;
      uv *= rotate2D(bassDrive * 0.01);
      vec2 uv0 = uv * zoom * .6;
      vec3 finalColor = vec3(0.0);
      
      for (float i = 0.0; i < amp; i++) {
        uv = fract(uv * 1.5) - 0.5;
        uv *= rotate2D(xfac * 0.1);
        float d = length(uv) * exp(-length(uv0));
  
        vec3 col = palette(length(uv0) * zoom + i*.5 + t*.5 + mid * 0.3);
  
        d = sin(d*8. + xfac)/8.0;
        d = abs(d);
  
        d = pow(0.1 / d, 0.1 + timeX);
  
        finalColor += col * d;
      }
  
      finalColor *= vec3(bass,mid,treble) * 0.1;
  
      gl_FragColor = vec4(finalColor, bassDrive);
    }
  `,
  
  pulsar: `
    uniform float time;
    uniform float timeX;
    uniform vec2 resolution;
    uniform float bubbles;
    uniform float bass;
    uniform float mid;
    uniform float treble;
    uniform float bassDrive;
    uniform float midDrive;
    uniform float trebleDrive;
    uniform float total;
    uniform float amp;
    varying vec2 vUv;
    uniform float zoom;
  
  vec3 hsv2rgb(vec3 c) {
      vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
      vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
      return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
  }
  
  mat2 rotate2D(float angle) {
      float s = sin(angle), c = cos(angle);
      return mat2(c, -s, s, c);
  }
  
  void main() {
      vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / min(resolution.x, resolution.y);
      uv = uv * (zoom * 1.5);
      
      vec3 color = vec3(0.0);
      float t = time * 0.05;  // Slowed down time factor
      float amplitude = (bass + mid + treble) * 0.9;  // Reduced amplitude
      float drivers = (bassDrive + midDrive + trebleDrive) * 0.002;
      float newBass = bass * 0.7;
      float newMid = mid * 0.7;
      float newTreble = treble * 0.7;
      
      for (float i = 0.0; i < amplitude; i++) {
          uv = abs(uv) / dot(uv, uv) - 0.8;
          uv *= rotate2D(drivers + i * 0.005);
          
          float d = length(uv) * exp(-length(uv));
          
          // Create a shifting color palette
          vec3 col1 = hsv2rgb(vec3(fract(t * 0.1 + i * 0.02 + newBass * 0.4), 0.7, 0.8));
          vec3 col2 = hsv2rgb(vec3(fract(t * 0.15 + i * 0.03 + newMid * 0.4), 0.8, 0.9));
          vec3 col = mix(col1, col2, sin(d * 10.0 + newTreble) * 0.5 + 0.5);
          
          d = sin(d * (12.0 + sin(t + i) * 4.0) + t * 2.0) * 0.5 + 0.5;
          d = pow(0.02 / d, 1.0 + sin(t * 0.2) * 0.5);
          
          color += col * d;
      }
      
      // Add a subtle glow effect
      color += vec3(0.1, 0.2, 0.3) * (sin(t * 0.5) * 0.5 + 0.5) * length(uv);
      
      // Adjust overall brightness and contrast
      color = pow(color * 0.075, vec3(0.6));
      
      // Create a pulsating alpha based on the audio
      float alpha = (0.6 + 0.4 * sin(t * 2.0)) * (newTreble * 0.3 + newMid * 0.2 + newBass * 0.1);
      
      gl_FragColor = vec4(color, alpha * .6);
  }
  `,
  
  shatter: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform vec2 bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

  vec3 palette(float t) {
    vec3 a = vec3(1.5, 1.5, 1.5);
    vec3 b = vec3(2.0, 2.0, 2.0);
    vec3 c = vec3(1.0, 1.0, 1.0);
    vec3 d = vec3(bassDrive, midDrive, trebleDrive);
    return a - b * cos(bassDrive / (c * t + d));
  }
  
  mat2 rotate2D(float angle) {
    float s = cos(angle / .4682), c = sin(angle / .2341);
    return mat2(-c, s, s, c);
}

  void main() {
    
    float t = time * 0.2;
    float xfac = total * 0.005;
    
    vec2 uv = (gl_FragCoord.xy * 2.0 - resolution.xy) / resolution.y;
    uv *= rotate2D(fract(xfac * 0.0041));
    vec2 uv0 = uv * -2.0;
    vec3 finalColor = vec3(0.0);
    
      for (float i = 0.0; i < amp; i++) {
        uv = fract(uv) - .5 ;
        uv *= rotate2D(xfac * 0.041);
        float d = length(uv) * exp(-length(uv0));
  
        vec3 col = palette(length(uv0) + t + i*.618 );
        
        // d = tan(d * 5. + zoom)/7.0;

        // Original abs version
        // d = abs(sin(d * (6.0 + amp)));

        // Experiment with different distance functions here
        // Uncomment one at a time to see the effect

        // 1. Fractional
        // d = fract(sin(d * (6.0 + amp)));

        // 2. Sine wave
        // d = 0.5 + 0.5 * sin(d * (6.0 + amp));

        // 3. Step function
        // d = step(0.5, sin(d * (6.0 + amp)));

        // 4. Smooth step
        // d = smoothstep(0.3, 0.7, sin(d * (6.0 + t)));

        // 5. Power function
        // d = pow(sin(d * (6.0 + amp)), 3.0);

        // 6. Exponential
        // d = 1.0 - exp(-sin(d * (6.0 + zoom)));

        // 7. Modulo
         d = mod(sin(d * (6.0 + amp)), timeX);

        // d = pow(0.01 / d, 1.);

        finalColor += col / d ;
    }

    finalColor *= vec3(0.01 + treble, 0.01 + bass, 0.01 + mid) * .0020;

    gl_FragColor = vec4(finalColor, treble * 0.085);
}
`,

lotus: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;


vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 3.0 / 2.0, 1.0 / 3.0, 2.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 1.618 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

mat2 rotate2D(float angle) {
    float s = sin(angle * timeX), c = cos(angle);
    return mat2(c, -s, s, c);
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / min(resolution.x, resolution.y);
    uv = uv * (zoom );
    
    vec3 color = vec3(0.0);
    float t = time * 0.1;
    float u = t * 0.05;
    float amplitude = (bass + mid + treble) * .9;
    float drivers = (bassDrive + trebleDrive) * .05;
    float xfac = total * 0.01;
    float newBass = bass * 0.7;
    float newMid = mid * 0.7;
    float newTreble = treble * 0.7;
    
    for (float i = 0.0; i < amplitude; i++) {
        uv = abs(uv) / dot(uv, uv) - 1.0;
        uv *= rotate2D(drivers * 0.1);
        
        float d = length(uv) * exp(-length(uv));
        
        vec3 col = hsv2rgb(vec3(fract( timeX  + (newBass * 0.2) ),(0.01 + (newMid * 0.1)), (0.01 + (newTreble *0.1) )));
        
        d = sin(d * (8.0  ) / 20. ) ;
     //   d = abs(d );
        d = pow(0.03 / d, timeX);
        
        color += col * d ;
    }
    
    //color *= 0.08;
    float trebAlpha = newTreble * .01;
    gl_FragColor = vec4(color, trebAlpha);
}
`,

vortex: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 2.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

mat2 rotate2D(float angle) {
    float s = sin(angle), c = tan(angle);
    return mat2(c, -s, s, c);
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / min(resolution.x, resolution.y);
    uv = uv * 3.;
    
    vec3 color = vec3(0.0);
    float t = time * 0.4;
    float u = 1. + (zoom * 0.8);
    float amplitude = bass + mid + treble;
    
    
    for (float i = 0.0; i < u; i++) {
        uv = abs(uv) / dot(uv, uv) - 1.0;
        uv *= rotate2D(t * 0.5);
        
        float d = length(uv) * exp(-length(uv));
        
        
        vec3 col = hsv2rgb(vec3(fract(t + i * 0.2 + (mid * 0.001)), .01 + timeX, 1.01 - timeX));
        
        d = sin(d * (30.0 ) + timeX) / 10.0;
        d = fract(d + (amplitude * .001));
        d = abs(d);
        d = pow(0.1 / d, timeX);
        
        color += col * d * (0.1 + (mid  * 0.02));
    }
    
  //  color *= zoom * 0.05;
    
    gl_FragColor = vec4(color, 0.70);
}
`,

bubbles: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

  vec3 palette(float t) {
  float m = treble * 0.1;
    vec3 a = vec3(m, m, m);
    vec3 b = vec3(0.5, 0.5, timeX * 0.6);
    vec3 c = vec3(timeX, 1.0, 1.0);
    vec3 d = vec3(0.263, 0.416, timeX * 0.6);
    return a + b * cos(3.28318 * (c * t + d));
  }

  void main() {
    vec2 uv = (gl_FragCoord.xy * 2.0 - resolution.xy) / resolution.y;
    vec2 uv0 = uv;
    vec3 finalColor = vec3(0.0);
    
    float t = time * 0.2;
    

    for (float i = 0.0; i < bubbles; i++) {
      uv = fract(uv * 1.618) - 0.618;

      float d = length(uv) * exp(-length(uv0));

      vec3 col = palette(length(uv0) + (bass * 0.3) + (mid * 0.01) + (treble * 0.01) + i*.2 + t*.2);

      d = sin(d * 8. + mid * 0.01)/2.;
      d = abs(d);

      d = pow(0.1 / d, .5);

      finalColor += col * d;
    }
    
    float alpha = treble * .06;
    finalColor *= vec3( mid, treble, bass) * alpha;

    gl_FragColor = vec4(finalColor, timeX * .8);
  }
`,

acid: `
uniform float time;
    uniform float bass;
    uniform float mid;
    uniform float treble;
    varying vec2 vUv;

    void main() {
        vec2 p = vUv * 2.0 - 1.0;
        float r = length(p);
        float a = atan(p.y, p.x);
        
        float f = 0.2 *  bass * sin(a * 10.0 + time) + 0.2 * mid * cos(r * 20.0 - time * 2.0) + 0.2 * treble * sin(r * 30.0 + time * 3.0);
        
        vec3 color = vec3(0.1 + (bass * .3) * cos(f + time), 0.1 + (mid * .3) * sin(f + time + 1.0), 0.1 + (treble * .3) * tan(f + time + 2.0));
        
        gl_FragColor = vec4(color, .4);
    }
`,

chasm: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

    void main() {
        vec2 p = vUv * 2.0 - 1.0;
        float r = length(p);
        float a = atan(p.y, p.x);
        float t = (time + bubbles) ; 
        float trebAlpha = treble * 0.1;
        
        float f = 0.1 * mid * cos(a * 13.0 + t) - 0.1 * treble * sin(r * 31.0 - t * .40) - 0.1 * bass * sin(r * 7.0 + t * 0.40);
        
        vec3 color = vec3(0.1 + (treble * .2) * cos(f + t), 0.2 + (bass * .2) * sin(f + t + 1.0), 0.2 + (mid * .2) * cos(f + t + 2.0));
        
        gl_FragColor = vec4(color, timeX);
    }
`,


storm: `
  uniform float time;
  uniform float timeX;
  uniform float timeI;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

    float random(vec2 st) {
        return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
      }

      vec2 rotate(vec2 v, float a) {
        float s = sin(a);
        float c = cos(a);
        mat2 m = mat2(c, -s, s, c);
        return m * v;
      }

      void main() {
        vec2 p = vUv * 2.0 - 1.0;
        float r = length(p);
        float a = atan(p.y, p.x);

        vec2 st = vec2(r, a);
        st = rotate(st, timeX);

        float cells = 10.0 + timeI * sin(time * 0.05);
        vec2 cellsPos = fract(st * cells) - 0.5;
        float cellDist = length(cellsPos)+treble*.1;

        float n = random(floor(st * cells));
        float size = 0.4 + 0.2 * sin(n * 6.28 + timeI + bassDrive*.05);

        float brightness = smoothstep(size, size * 0.8, cellDist);
        brightness *= 1.0 - smoothstep(0.8, 1.5, r + 0.2 * sin(a * timeI + time + treble*.3));

        vec3 color = vec3(
          0.618 + 0.618 * sin(n * 6.28 + timeI * 0.7 + bass * .3),
          0.618 + 0.618 * sin(n * 6.28 + timeI * 0.5 + mid * .3),
          0.618 + 0.618 * sin(n * 6.28 + timeI * 0.3 + treble * .6)
        );

        color *= brightness;
        color += vec3(0.2, 0.05, 0.4) * (1.0 - r);

        gl_FragColor = vec4(color, .4);
    }
`,


nexus: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

mat2 rotate2D(float angle) {
    float s = sin(angle), c = cos(angle);
    return mat2(c, -s, s, c);
}

float noise(vec2 p) {
    return fract(sin(dot(p.xy, vec2(12.9898, 78.233))) * 43758.5453);
}

vec2 spiralDistort(vec2 uv, float t) {
    float r = length(uv);
    float theta = atan(uv.y, uv.x);
    float spiral = r + 0.5 * sin(theta * 3.0 + t);
    return vec2(spiral * cos(theta), spiral * sin(theta));
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / min(resolution.x, resolution.y);
    uv = uv * (zoom * 1.5);
    
    vec3 color = vec3(0.0);
    float t = time * 0.05;
    float longTermT = time * 0.005;
    float amplitude = (bass + mid + treble) * 0.8;
    float drivers = (bassDrive + midDrive + trebleDrive) * 0.02;
    
    float evolutionFactor = sin(longTermT) * 0.3 + 0.5;
    float shapeFactor = cos(longTermT * 0.5) * 0.5 + 0.5;
    
    // Evolving rotation speed and direction
    float rotationSpeed = sin(longTermT * 0.05) * 0.5 + 0.5;
    float rotationDirection = sign(sin(longTermT * 0.7));
    
    // Evolving movement pattern
    float movementPattern = mod(floor(longTermT * 0.1), 2.0); // 0, 1, or 2
    
    for (float i = 0.0; i < amplitude; i++) {
        float shapeEvolution = mix(0.8, 1.2, shapeFactor);
        
        // Apply different movement patterns
        if (movementPattern < 1.0) {
            // Spiral distortion
            uv = spiralDistort(uv, t * rotationSpeed * rotationDirection);
        } else if (movementPattern < 2.0) {
            // Wavy distortion
            uv += vec2(sin(uv.y * 3.0 + t), cos(uv.x * 3.0 + bassDrive*.1)) * 0.1;
        } else {
            // Ripple distortion
            float ripple = sin(length(uv) * treble - t * 2.0) * 0.1;
            uv += normalize(uv) * ripple;
        }
        
        uv = abs(uv) / dot(uv, uv) - shapeEvolution;
        
        // Complex rotation
        float rotationAngle = drivers + i * 0.005 + longTermT * 0.1;
        rotationAngle += sin(length(uv) * 5.0 + t) * 0.2; // Add some wave to the rotation
        uv *= rotate2D(rotationAngle * rotationSpeed * rotationDirection);
        
        float d = length(uv) * exp(-length(uv));
        
        vec3 col1 = hsv2rgb(vec3(fract(t * 0.1 + i * 0.02 + bass * 0.1 + longTermT), 0.7, 0.8));
        vec3 col2 = hsv2rgb(vec3(fract(t * 0.15 + i * 0.03 + treble * 0.1 - longTermT), 0.8, 0.9));
        vec3 col = mix(col1, col2, sin(d * 10.0 + t) * 0.5 + 0.5);
        
        float patternEvolution = mix(8.0, 16.0, evolutionFactor);
        d = sin(d * (patternEvolution + sin(t + i) * 4.0) + t * 2.0) * 0.5 + 0.5;
        d = abs(d);
        d = pow(0.02 / d, 1.0 + sin(t * 0.2 + longTermT) * 0.5);
        
        color += col * d;
    }
    
    vec3 glowColor = mix(vec3(0.1, 0.2, 0.3), vec3(0.3, 0.1, 0.2), evolutionFactor);
    color += glowColor * (sin(t * 0.5) * 0.5 + 0.5) * length(uv);
    
    float noiseValue = noise(uv * 10.0 + vec2(longTermT));
    color = mix(color, color * noiseValue, evolutionFactor * 0.3);
    
    color = pow(color * 0.075, vec3(0.8));
    
    float alpha = (0.6 + 0.4 * sin(t * 2.0 + longTermT)) * (treble * 0.3 + mid * 0.2 + bass * 0.1);
    alpha = mix(alpha, alpha * noiseValue, evolutionFactor * 0.5);
    
    gl_FragColor = vec4(color, alpha * .7);
}
`,

scorpion: `
  uniform float time;
  uniform float timeX;
  uniform float timeI;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

mat2 rotate2D(float angle) {
    float s = sin(angle), c = cos(angle);
    return mat2(c, -s, s, c);
}

float noise(vec2 p) {
    return fract(sin(dot(p.xy, vec2(12.9898, 78.233))) * 43758.5453);
}

vec2 spiralDistort(vec2 uv, float t) {
    float r = length(uv);
    float theta = atan(uv.y, uv.x);
    float spiral = r + 0.5 * sin(theta * 3.0 + t);
    return vec2(spiral * cos(theta), spiral * sin(theta));
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / min(resolution.x, resolution.y);
    uv = uv * (zoom * .5);
    
    vec3 color = vec3(0.0);
    float t = time * 0.05;
    float longTermT = time * 0.01;
    float amplitude = (bass + mid + treble) * 0.8;
    float drivers = (bassDrive + midDrive + trebleDrive) * 0.07;
    
    float evolutionFactor = sin(longTermT) * 0.618 + 0.5;
    float shapeFactor = cos(longTermT * 0.2) * 0.5 + 0.5;
    
    // Evolving rotation speed and direction
    float rotationSpeed = sin(longTermT * 0.3) * 0.5 + 0.5;
    float rotationDirection = sign(sin(longTermT * 0.618));
    
    // Evolving movement pattern
    float movementPattern = mod(floor(longTermT * 0.3), zoom); // 0, 1, or 2
    
    for (float i = 0.0; i < timeI; i++) {
        float shapeEvolution = mix(0.8, 1.2, shapeFactor);
        
        // Apply different movement patterns
        if (movementPattern < 1.0) {
            // Spiral distortion
            uv = spiralDistort(uv, t * rotationSpeed * rotationDirection);
        } else if (movementPattern < 2.0) {
            // Wavy distortion
            uv += vec2(sin(uv.y * 5.0 + t), cos(uv.x * 5.0 + t)) * 0.1;
        } else {
            // Ripple distortion
            float ripple = sin(length(uv) * 7.0 - t * 2.0) * 0.1;
            uv += normalize(uv) * ripple;
        }
        
        uv = abs(uv) / dot(uv, uv) - shapeEvolution;
        
        // Complex rotation
        float rotationAngle = drivers + i * 0.1 + longTermT * 0.1;
        rotationAngle += sin(length(uv) * timeX + t) * 0.06; // Add some wave to the rotation
        uv *= rotate2D(rotationAngle * rotationSpeed * rotationDirection);
        
        float d = length(uv) * exp(-length(uv));
        
        vec3 col1 = hsv2rgb(vec3(fract(t * 0.1 + i * 0.2 + bass * 0.4 + longTermT), 0.7, 0.8));
        vec3 col2 = hsv2rgb(vec3(fract(t * 0.15 + i * 0.3 + treble * 0.4 - longTermT), 0.8, 0.9));
        vec3 col = mix(col1, col2, sin(d * 10.0 + t) * 0.5 + 0.5);
        
        float patternEvolution = mix(8.0, 16.0, evolutionFactor);
        d = sin(d * (patternEvolution + sin(t + i) * 8.0) + t * 2.0) * 0.5 + 0.5;
      //  d = abs(d);
        d = pow(0.07 / d, 1.0 + sin(t * 0.2 + longTermT) * 0.3);
        
        color += col * d;
    }
    
    vec3 glowColor = mix(vec3(0.1, 0.2, 0.3), vec3(0.3, 0.1, 0.2), evolutionFactor);
    color += glowColor * (sin(t * 0.3) * 0.3 + 0.3) * length(uv);
    
    float noiseValue = noise(uv * 10.0 + vec2(longTermT));
    color = mix(color, color * noiseValue, evolutionFactor * 0.3);
    
    color = pow(color * 0.075, vec3(0.8));
    
    float alpha = (0.6 + 0.4 * sin(t * 2.0 + longTermT)) * (treble * 0.03 + mid * 0.02 + bass * 0.01);
    alpha = mix(alpha, alpha * noiseValue, evolutionFactor * 0.3);
    
    gl_FragColor = vec4(color, alpha * .7);
}
`,

mist: `
  uniform float time;
  uniform vec2 resolution;
  // Direct audio levels
  uniform float bass;
  uniform float mid;
  uniform float treble;
  // Cumulative drives for smooth evolution
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  // Optional uniforms
  uniform float total; // Could influence overall brightness/complexity
  uniform float zoom;  // Control field of view / grid density

  varying vec2 vUv;

  #define PI 3.14159265359
  #define TAU (2.0 * PI)

  // --- Helper Functions ---

  // Rotation
  mat2 rot2D(float angle) {
      float s = sin(angle);
      float c = cos(angle);
      return mat2(c, -s, s, c);
  }

  // Smooth minimum function (for blending line thicknesses)
  float smin(float a, float b, float k) {
      float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
      return mix(b, a, h) - k * h * (1.0 - h);
  }

  // Simple hash for randomness
  float hash(float n) {
      return fract(sin(n) * 43758.5453);
  }

  // HSV to RGB
  vec3 hsv2rgb(vec3 c) {
      vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
      vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
      return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
  }


  // --- Main Shader ---
  void main() {
      // Normalized coords, center origin, aspect correct
      vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / resolution.y;
      uv *= (1.0 + zoom * 0.3); // Apply zoom centered

      // --- Additive Evolution ---
      // 1. Rotation based on Treble Drive
      float gridAngle = trebleDrive * 0.001; // Slow rotation accumulation
      uv = rot2D(gridAngle) * uv;

      // 2. Warping based on Bass Drive (like a heat haze)
      float warpIntensity = bassDrive * 0.00005; // Very subtle cumulative warp
      float warp = sin(uv.y * 5.0 + time * 0.1) * cos(uv.x * 4.0 - time * 0.05);
      uv += warp * warpIntensity * vec2(0.5, 1.0); // Anisotropic warp

      // 3. Grid Frequency based on Mid Drive
      float baseFreq = 4.0; // Base number of lines
      float freqMultiplier = midDrive * 0.0008; // Mid slowly increases complexity
      float gridFreq = baseFreq + freqMultiplier;

      // --- Grid Line Calculation ---
      // Wave amplitudes modulated by current Mid level
      float waveAmpX = 0.05 + mid * 0.05; // Horizontal line wiggle
      float waveAmpY = 0.05 + mid * 0.05; // Vertical line wiggle

      // Calculate wave patterns for horizontal and vertical lines
      float waveX = sin(uv.y * gridFreq * PI + time * 0.5 + sin(uv.x * 2.0 + time) * waveAmpY);
      float waveY = cos(uv.x * gridFreq * PI + time * 0.5 + cos(uv.y * 2.0 + time) * waveAmpX);

      // Calculate distance to the nearest grid line (using smoothstep for anti-aliasing)
      float lineThicknessBase = 0.01;
      // Bass makes lines glow thicker
      float lineThickness = lineThicknessBase + bass * 0.02;

      // Smoothstep creates soft edges for the lines
      float gridX = smoothstep(lineThickness, 0.0, abs(waveX));
      float gridY = smoothstep(lineThickness, 0.0, abs(waveY));

      // Combine horizontal and vertical lines (take the max brightness)
      float gridValue = max(gridX, gridY);

      // --- Intersection Calculation ---
      // Intersections are where both gridX and gridY are high
      float intersectionGlow = gridX * gridY;
      // Boost intensity significantly based on current Treble
      intersectionGlow = pow(intersectionGlow, 2.0) * (1.0 + treble * 5.0);

      // --- Coloring ---
      // Base color cycles slowly with time
      float baseHue = fract(time * 0.03);
      vec3 gridColor = hsv2rgb(vec3(baseHue, 0.8, 0.8)); // Vibrant base color for lines

      // Bass glow color (e.g., a warm orange/red)
      vec3 bassGlowColor = hsv2rgb(vec3(0.05 + fract(baseHue + 0.1), 0.9, 1.0));
      // Apply bass glow additively based on bass level and line proximity
      vec3 finalColor = gridColor * gridValue;
      finalColor += bassGlowColor * gridValue * (bass * 0.5); // Additive glow

      // Intersection color (shifts rapidly with treble)
      float intersectionHue = fract(baseHue + treble * 0.3 + time); // Fast hue shift for intersections
      vec3 intersectionColor = hsv2rgb(vec3(intersectionHue, 0.5, 1.0)); // Bright, slightly desaturated flare
      // Add intersection glow
      finalColor = max(finalColor, intersectionColor * intersectionGlow); // Use max to make flares overwrite

      // --- Treble Static/Noise Effect ---
      // Generate simple noise based on UV and time
      float staticNoise = hash(dot(uv, uv) + time * 10.0);
      // Make noise pulse strongly with treble
      staticNoise *= smoothstep(0.3, 0.7, treble); // Only appears strongly with high treble
      staticNoise *= 0.3; // Keep it somewhat subtle
      // Add static as white noise overlay
      finalColor += vec3(staticNoise);

      // --- Final Output ---
      // Fade out towards edges slightly
      finalColor *= (1.0 - length(uv) * 0.3);

      // Clamp color values
      finalColor = clamp(finalColor, 0.0, 1.0);

      // Alpha depends on grid intensity and intersection glow
      float alpha = clamp(gridValue + intersectionGlow * 0.5 + bass * 0.1, 0.0, 0.9);

      gl_FragColor = vec4(finalColor, alpha);
  }

`,

wave: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float timeI;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  varying vec2 vUv;
  uniform float zoom;

    #define PI 3.14159265359

      float random(vec2 st) {
        return fract(sin(dot(st.xy, vec2(timeI*2., 78.233))) * 43758.5453123);
      }
      
      void main() {
        vec2 st = vUv * 2.0 - 1.0;
        float r = length(st);
        float a = atan(st.y, st.x);
        
        float noise = random(st + bassDrive * 0.01);
        float bassWave = sin(r * 10.0 - timeX * 2.0) * bass*.5;
        float midWave = cos(a * 8.0 + timeX * 1.5) * mid*.4;
        float trebleWave = sin(r * 15.0 + a * 3.0 + timeX) * treble*.2;
        
        float wave = bassWave + midWave + trebleWave;
        
        vec3 color = vec3(0.5 + 0.5 * sin(wave * 5.0 + time),
                          0.5 + 0.5 * cos(wave * 3.0 - time * 0.5),
                          0.5 + 0.5 * sin(wave * 7.0 + time * 0.7));
        
        color += vec3(noise * 0.1);
        
        gl_FragColor = vec4(color, .3);
}
`,

nebulus: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  uniform float zoom;

  varying vec2 vUv;
  
  #define PI 3.14159265359
  #define MAX_STEPS 100
  #define MAX_DIST 100.0
  #define SURF_DIST 0.001

  // Pseudo-random function - efficient hash
  float hash(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
  }

  // Rotation matrix
  mat2 rot2D(float angle) {
      float s = sin(angle);
      float c = cos(angle);
      return mat2(c, -s, s, c);
  }

  // Distance function for our tunnel
  float getTunnelDist(vec3 p) {
      float twistAngle = p.z * 0.05 + bassDrive * 0.1;
      p.xy *= rot2D(twistAngle);
      
      float tunnel = length(p.xy) - (1.0 + mid * 0.5);
      float waves = sin(p.z * 2.0 + time) * 0.1 * treble;
      waves += cos(atan(p.y, p.x) * 3.0 + time) * 0.1 * mid;
      
      return tunnel + waves;
  }

  // Ray marching function
  float rayMarch(vec3 ro, vec3 rd) {
      float dO = 0.0;
      
      for(int i = 0; i < MAX_STEPS; i++) {
          vec3 p = ro + rd * dO;
          float dS = getTunnelDist(p);
          dO += dS;
          if(dO > MAX_DIST || abs(dS) < SURF_DIST) break;
      }
      
      return dO;
  }

  // Get surface normals for lighting
  vec3 getNormal(vec3 p) {
      float d = getTunnelDist(p);
      vec2 e = vec2(0.01, 0);
      vec3 n = d - vec3(
          getTunnelDist(p - e.xyy),
          getTunnelDist(p - e.yxy),
          getTunnelDist(p - e.yyx)
      );
      return normalize(n);
  }

  // Color function based on position and audio
  vec3 getColor(vec3 p) {
      vec3 col = vec3(0.5);
      
      float bands = sin(p.z * 2.0 + bassDrive) * 0.5 + 0.5;
      bands *= sin(atan(p.y, p.x) * 8.0 + time) * 0.5 + 0.5;
      
      vec3 a = vec3(0.8, 0.5, 0.4);    // Warm base color
      vec3 b = vec3(0.2, 0.5, 0.8);    // Cool contrast
      vec3 c = vec3(1.0, 0.7, 0.4);    // Golden highlights
      vec3 d = vec3(
          bass * 0.5 + sin(time * 0.2) * 0.5,    // Pulsing red
          mid * 0.5 + cos(time * 0.3) * 0.5,     // Flowing green
          treble * 0.5 + sin(time * 0.4) * 0.5   // Swimming blue
      );
      
      col = a + b * cos(2.0 * PI * (c * bands + d + time * 0.1));
      
      return col;
  }

  void main() {
      vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / resolution.y;
      
      // REVERSED DIRECTION: negative time factor for moving toward viewer
      // Slower speed for smoother movement
      vec3 ro = vec3(0, 0, 8.0 + time * 0.8 * (1.0 + bass * 0.2));
      vec3 rd = normalize(vec3(uv * (1.0 + zoom * 0.15), -1.0)); // Note: negative z direction
      
      rd.xy *= rot2D(mid * 0.2);
      rd.xz *= rot2D(treble * 0.1);
      
      float d = rayMarch(ro, rd);
      vec3 p = ro + rd * d;
      
      vec3 n = getNormal(p);
      
      // Move light source to better illuminate the tunnel coming toward us
      vec3 lightPos = vec3(2, 2, ro.z - 5.0);
      vec3 l = normalize(lightPos - p);
      float diff = max(0.0, dot(n, l));
      
      vec3 col = getColor(p);
      
      // Basic lighting
      col *= diff;
      
      // PARTICLE EFFECT: Calculate proximity factor - MUCH more focused now
      // Only start dissolving when very close to camera (3.0-2.0 range)
      float proximity = smoothstep(3.0, 2.0, distance(p, ro));
      
      // Generate more subtle noise patterns
      float noise1 = hash(p.xy * 5.0 + time * 0.3);
      float noise2 = hash(p.yz * 4.0 - time * 0.4);
      float noise3 = hash(p.xz * 7.0 + time * 0.6);
      
      // More balanced noise combination
      float particleNoise = (noise1 * 0.5 + noise2 * 0.3 + noise3 * 0.2) * 0.4;
      
      // More controlled audio factor
      float audioFactor = bass * 0.2 + mid * 0.15 + treble * 0.15;
      float particleEffect = particleNoise * proximity * (0.5 + audioFactor);
      
      // Create more subtle particle color based on audio frequencies - reduced intensity
      vec3 particleColor = vec3(0.7, 0.4, 0.2) * bass * 0.5 + 
                          vec3(0.2, 0.4, 0.7) * treble * 0.5 + 
                          vec3(0.5, 0.2, 0.4) * mid * 0.5;
      
      // Apply distance attenuation
      col *= 1.0 - d * 0.02;
      
      // Apply particle effect using mix - much more subtle max value (0.4 vs 0.9)
      col = mix(col, particleColor, clamp(particleEffect, 0.0, 0.4));
      
      // Add more subtle glow for particles
      col += particleColor * particleEffect * audioFactor * 0.15;
      
      // Add rim highlighting on edges - more controlled
      float rimLight = pow(1.0 - abs(dot(n, rd)), 4.0) * proximity * 0.5;
      col += particleColor * rimLight * audioFactor * 0.3;
      
      // Add very subtle dust particles in the tunnel
      float dustNoise = hash(uv * 20.0 + time * 0.1) * 0.08;
      col += dustNoise * particleColor * (1.0 - proximity) * 0.1;
      
      gl_FragColor = vec4(col, 0.7);
  }
`,

nebulus2: `
  uniform float time;
  uniform float timeX;
  uniform vec2 resolution;
  uniform float bubbles;
  uniform float bass;
  uniform float mid;
  uniform float treble;
  uniform float bassDrive;
  uniform float midDrive;
  uniform float trebleDrive;
  uniform float total;
  uniform float amp;
  uniform float zoom;

  varying vec2 vUv;
  
  #define PI 3.14159265359
  #define MAX_STEPS 100
  #define MAX_DIST 100.0
  #define SURF_DIST 0.001

  // Rotation matrix for our "camera"
  mat2 rot2D(float angle) {
      float s = sin(angle);
      float c = cos(angle);
      return mat2(c, -s, s, c);
  }

  // Distance function for our tunnel
  float getTunnelDist(vec3 p) {
      float twistAngle = p.z * 0.05 + bassDrive * 0.01;
      p.xy *= rot2D(twistAngle);
      
      float tunnel = length(p.xy) - (1.0 + mid * 0.5);
      float waves = sin(p.z * 2.0 + time) * 0.1 * (treble * 0.3);
      waves += cos(atan(p.y, p.x) * 3.0 + time) * 0.1 * (mid * .2);
      
      return tunnel + waves;
  }

  // Ray marching function
  float rayMarch(vec3 ro, vec3 rd) {
      float dO = 0.0;
      
      for(int i = 0; i < MAX_STEPS; i++) {
          vec3 p = ro + rd * dO;
          float dS = getTunnelDist(p);
          dO += dS;
          if(dO > MAX_DIST || abs(dS) < SURF_DIST) break;
      }
      
      return dO;
  }

  // Get surface normals for lighting
  vec3 getNormal(vec3 p) {
      float d = getTunnelDist(p);
      vec2 e = vec2(0.01, 0);
      vec3 n = d - vec3(
          getTunnelDist(p - e.xyy),
          getTunnelDist(p - e.yxy),
          getTunnelDist(p - e.yyx)
      );
      return normalize(n);
  }

  // Color function based on position and audio
  vec3 getColor(vec3 p) {
      vec3 col = vec3(0.5);
      
      float bands = sin(p.z * 2.0 + (bassDrive * .3)) * 0.5 + 0.5;
      bands *= sin(atan(p.y, p.x) * 8.0 + time) * 0.5 + 0.5;
      
      vec3 a = vec3(0.8, 0.5, 0.4);    // Warm base color
      vec3 b = vec3(0.2, 0.5, 0.8);    // Cool contrast
      vec3 c = vec3(1.0, 0.7, 0.4);    // Golden highlights
      vec3 d = vec3(
          bass * 0.5 + sin(time * 0.2) * 0.5,    // Pulsing red
          mid * 0.5 + cos(time * 0.3) * 0.5,     // Flowing green
          treble * 0.5 + sin(time * 0.4) * 0.5   // Swimming blue
      );
      
      col = a + b * cos(2.0 * PI * (c * bands + d + time * 0.1));
      
      return col;
  }

  void main() {
      vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / resolution.y;
      
      vec3 ro = vec3(0, 0, -3.0 + time * 0.5 * (1.0 + bass * 0.3));
      vec3 rd = normalize(vec3(uv * (1.0 + zoom * 0.2), 1.0));
      
      rd.xy *= rot2D(mid * 0.2);
      rd.xz *= rot2D(treble * 0.1);
      
      float d = rayMarch(ro, rd);
      vec3 p = ro + rd * d;
      
      vec3 n = getNormal(p);
      
      vec3 lightPos = vec3(2, 2, -2);
      vec3 l = normalize(lightPos - p);
      float diff = max(0.0, dot(n, l));
      
      vec3 col = getColor(p);
      
      col *= diff;
      col *= 1.0 - d * 0.1;
      
      col += vec3(0.1, 0.2, 0.4) * treble * 0.5;
      
      gl_FragColor = vec4(col, 0.7);
  }
`,

prismatic: `
uniform float time;
uniform float timeX;
uniform float timeI;
uniform vec2 resolution;
uniform float bass;
uniform float mid;
uniform float treble;
uniform float bassDrive;
uniform float midDrive;
uniform float trebleDrive;
uniform float total;
uniform float amp;
uniform float zoom;
uniform float colorSpeed;
uniform float glowIntensity;

varying vec2 vUv;

#define PI 3.14159265359
#define TAU 6.28318530718
#define MAX_ECHO_LAYERS 3.0

// Smoothing function for transitions
float smoothValue(float value, float smoothness) {
  return smoothstep(0.0, 1.0, smoothstep(0.0, 1.0, value * smoothness) / smoothness);
}

// Rotation matrix
mat2 rot(float a) {
  float c = cos(a), s = sin(a);
  return mat2(c, -s, s, c);
}

// Hash function for randomness
float hash(vec2 p) {
  p = fract(p * vec2(123.45, 678.91));
  p += dot(p, p + 33.33);
  return fract(p.x * p.y);
}

// Get journey progress (0-1 range that cycles very slowly)
float getJourneyProgress() {
  // Make a full journey take about 2 minutes
  return fract(time * 0.008);
}

// Get scene index (which part of the journey we're in)
float getSceneIndex() {
  // Break journey into 5 scenes
  float progress = getJourneyProgress();
  return floor(progress * 5.0);
}

// Scene transition blend factor (0-1)
float getSceneTransition() {
  float progress = getJourneyProgress();
  float sceneLength = 1.0 / 5.0; // 5 scenes
  return fract(progress * 5.0) / sceneLength;
}

// Generates neon line patterns
float neonLines(vec2 uv, float thickness, float brightness, float speed, float offset) {
  // Get journey/scene info
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  float transition = getSceneTransition();
  
  // Create animated line pattern
  float linePattern = 0.0;
  
  // Line properties vary by scene
  float lineCount = 3.0;
  float lineDirection = 1.0;
  
  // Scene-specific line variations
  if (scene < 1.0) {
    // Scene 0: Horizontal lines coming in
    lineCount = 2.0 + transition * 2.0;  // Lines appear gradually
    lineDirection = 1.0;  // Horizontal focus
  } else if (scene < 2.0) {
    // Scene 1: Grid forming
    lineCount = 4.0;
    lineDirection = mix(1.0, 0.0, transition); // Transition to vertical
  } else if (scene < 3.0) {
    // Scene 2: Diagonal streaks
    lineCount = 4.0 + transition * 2.0;  // More lines
    lineDirection = -1.0; // Diagonal focus
  } else if (scene < 4.0) {
    // Scene 3: Bursting outward
    lineCount = 6.0;
    lineDirection = mix(-1.0, 0.3, transition); // Changing angle
  } else {
    // Scene 4: Converging to center
    lineCount = 6.0 - transition * 3.0; // Lines disappearing
    lineDirection = mix(0.3, 1.0, transition); // Back to horizontal for loop
  }
  
  // Several layers of lines at different angles and frequencies
  for (float i = 0.0; i < lineCount; i++) {
    float angleOffset = i * PI / lineCount + offset;
    angleOffset *= lineDirection; // Direction control
    
    vec2 rotatedUV = rot(angleOffset) * uv;
    
    // Scene-specific pattern variations
    float freqMultiplier = 1.0;
    float waveShape = 0.0;
    
    if (scene < 1.0) {
      // Scene 0: Simple sine waves
      freqMultiplier = mix(5.0, 10.0, transition);
      waveShape = sin(rotatedUV.x * freqMultiplier + (trebleDrive * 0.01) * speed);
    } else if (scene < 2.0) {
      // Scene 1: Grid pattern forming
      freqMultiplier = 10.0 + i * 2.0;
      waveShape = sin(rotatedUV.x * freqMultiplier) * cos(rotatedUV.y * (5.0 + i));
    } else if (scene < 3.0) {
      // Scene 2: Diagonal ripples
      freqMultiplier = 12.0 + i * 5.0;
      waveShape = sin(rotatedUV.x * freqMultiplier + rotatedUV.y * freqMultiplier + time * speed);
    } else if (scene < 4.0) {
      // Scene 3: Radial pattern
      float dist = length(rotatedUV);
      waveShape = sin(dist * (15.0 - i * 2.0) - time * speed);
    } else {
      // Scene 4: Fractal-like complexity to transition back
      freqMultiplier = 15.0 - i * 3.0;
      waveShape = sin(rotatedUV.x * freqMultiplier) * sin(rotatedUV.y * freqMultiplier * 0.5);
    }
    
    // Apply scene-specific distortion
    if (scene == 1.0 && transition > 0.5) {
      waveShape *= sin(rotatedUV.y * 3.0 + time * 0.2);
    } else if (scene == 3.0) {
      waveShape *= sin(length(rotatedUV) * 5.0 - time * 0.3);
    }
    
    // Make the lines sharper with smoothstep
    float lines = smoothstep(1.0 - thickness, 1.0, abs(waveShape));
    
    // Add to total pattern with decreasing intensity for each layer
    linePattern += lines * brightness * (1.0 - i / lineCount);
  }
  
  return linePattern;
}

// Generate more dynamic kaleidoscopic folding
vec2 journeyKaleido(vec2 p, float segments, float intensity) {
  // Get journey progress
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  
  // Scene-specific kaleidoscope behaviors
  if (scene < 1.0) {
    // Scene 0: Very subtle kaleidoscope
    intensity *= 0.2 + journey * 0.8;
    segments = mix(2.0, 4.0, journey);
  } else if (scene < 2.0) {
    // Scene 1: Stronger kaleidoscope effect
    intensity *= 0.5 + sin(journey * TAU) * 0.5;
    segments = mix(4.0, 6.0, fract(journey * 5.0));
  } else if (scene < 3.0) {
    // Scene 2: Pulsing kaleidoscope
    intensity = 0.5 + 0.5 * sin(time * 0.2);
    segments = 6.0 + floor(sin(time * 0.1) * 2.0);
  } else if (scene < 4.0) {
    // Scene 3: Maximum kaleidoscope
    intensity = 0.8 + 0.2 * sin(time * 0.3);
    segments = 8.0;
  } else {
    // Scene 4: Gradually returning to minimal
    intensity = mix(0.8, 0.1, fract(journey * 5.0));
    segments = mix(8.0, 2.0, fract(journey * 5.0));
  }
  
  // Apply the kaleidoscope effect
  float angle = TAU / segments;
  float a = atan(p.y, p.x) + angle * 0.5;
  a = mix(a, mod(a, angle) - angle * 0.5, intensity);
  return length(p) * vec2(cos(a), sin(a));
}

// Color palette for neon effects with journey progression
vec3 journeyPalette(float t, float saturation, float brightness) {
  // Journey progress affects the color palette
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  
  // Scene-specific color schemes
  vec3 c, d;
  
  if (scene < 1.0) {
    // Scene 0: Cool blue to purple
    c = vec3(1.0, 0.5, 0.3);
    d = vec3(0.0, 0.1, 0.2);
  } else if (scene < 2.0) {
    // Scene 1: Purples to pinks
    c = vec3(0.8, 0.5, 0.4);
    d = vec3(0.2, 0.1, 0.3);
  } else if (scene < 3.0) {
    // Scene 2: Cyberpunk (teal and pink)
    c = vec3(0.9, 0.6, 0.3);
    d = vec3(0.1, 0.3, 0.2);
  } else if (scene < 4.0) {
    // Scene 3: Hot colors (oranges to reds)
    c = vec3(0.7, 0.6, 0.5);
    d = vec3(0.2, 0.0, 0.0);
  } else {
    // Scene 4: Cool down (blue to green)
    c = vec3(0.7, 0.8, 0.5);
    d = vec3(0.3, 0.2, 0.1);
  }
  
  // Extra variation based on journey progress
  d = fract(d + journey * 0.2);
  
  // Build the color
  vec3 a = vec3(0.5, 0.5, 0.5) * brightness;
  vec3 b = vec3(0.5, 0.5, 0.5) * saturation;
  
  // Final color with some time animation
  return a + b * cos(TAU * (c * t + d + time * colorSpeed * 0.05));
}

// Echo/delay system - returns time-delayed values
float echo(float value, float decay, float index) {
  // Create a delayed version of the value
  float delay = index * 0.5; // half-second delay between each echo
  float timeOffset = time - delay;
  float delayedValue = value * pow(decay, index); // Exponential decay
  
  // Add some oscillation to make it more interesting
  delayedValue *= 0.8 + 0.2 * sin(timeOffset * 0.618);
  
  return delayedValue;
}

// Simplified pattern function for background
float journeyBackground(vec2 p, float layer) {
  // Get journey progress
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  
  // Echo parameters
  float echoDelay = layer * 1.2;
  float t = time * 0.1 - echoDelay;
  
  // Audio reactivity with echo
  float echoDecay = 0.7;
  float bassFactor = echo(bass * 0.4, echoDecay, layer);
  float midFactor = echo(mid * 0.4, echoDecay, layer);
  float trebleFactor = echo(treble * 0.6, echoDecay, layer);
  
  // Scene-specific backgrounds
  float pattern = 0.0;
  
  if (scene < 1.0) {
    // Scene 0: Simple gradient rings
    pattern = 0.5 + 0.5 * sin(length(p) * 5.0 - t - bassFactor);
  } else if (scene < 2.0) {
    // Scene 1: Cross-hatch pattern
    pattern = 0.5 + 0.25 * sin(p.x * 10.0 + t) + 0.25 * cos(p.y * 10.0 - t);
  } else if (scene < 3.0) {
    // Scene 2: Swirling pattern
    float angle = atan(p.y, p.x);
    pattern = 0.5 + 0.5 * sin(angle * 5.0 + length(p) * 10.0 - t * 2.0);
  } else if (scene < 4.0) {
    // Scene 3: Wave interference pattern
    pattern = 0.5 + 0.25 * sin(p.x * 8.0 - t) * sin(p.y * 8.0 - t);
  } else {
    // Scene 4: Return to simple pattern for loop
    pattern = 0.5 + 0.5 * sin(dot(p, p) * 10.0 + t);
  }
  
  // Add audio reactivity
  pattern += bassFactor * 0.1 * sin(length(p) * 20.0 - t * 5.0);
  pattern += midFactor * 0.1 * sin(p.x * p.y * 10.0 - t * 2.0);
  
  return pattern;
}

// Parallax effect with journey progression
vec3 journeyParallax(vec2 uv, float layers) {
  vec3 result = vec3(0.0);
  float totalWeight = 0.0;
  
  // Journey progress
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  
  // Scene-specific parallax behavior
  float moveSpeed = 0.1;
  float layerSeparation = 0.15;
  
  if (scene < 1.0) {
    // Scene 0: Gentle parallax
    moveSpeed = mix(0.05, 0.1, journey * 5.0);
    layerSeparation = mix(0.1, 0.15, journey * 5.0);
  } else if (scene < 2.0) {
    // Scene 1: Faster movement
    moveSpeed = 0.15;
    layerSeparation = 0.2;
  } else if (scene < 3.0) {
    // Scene 2: Maximum depth
    moveSpeed = 0.2;
    layerSeparation = 0.25;
  } else if (scene < 4.0) {
    // Scene 3: Swirling parallax
    moveSpeed = 0.15;
    layerSeparation = 0.2;
  } else {
    // Scene 4: Slowing down
    moveSpeed = mix(0.15, 0.05, fract(journey * 5.0));
    layerSeparation = mix(0.2, 0.1, fract(journey * 5.0));
  }
  
  // Generate multiple layers with varying depth
  for (float i = 0.0; i < 3.0; i++) {
    // Scale factor creates parallax effect - closer layers appear to move faster
    float depth = 1.0 + i * layerSeparation;
    float scale = 1.0 / depth;
    
    // Scene-specific movement patterns
    vec2 offset;
    
    if (scene < 1.0) {
      // Scene 0: Left to right motion
      offset = vec2(time * moveSpeed, 0.0);
    } else if (scene < 2.0) {
      // Scene 1: Diagonal motion
      offset = vec2(sin(time * moveSpeed), cos(time * moveSpeed * 0.5));
    } else if (scene < 3.0) {
      // Scene 2: Circular motion
      float angle = time * moveSpeed * 0.5;
      offset = vec2(sin(angle), cos(angle)) * moveSpeed;
    } else if (scene < 4.0) {
      // Scene 3: Spiraling inward
      float angle = time * moveSpeed;
      float radius = 1.0 - fract(journey * 5.0) * 0.5;
      offset = vec2(sin(angle), cos(angle)) * radius;
    } else {
      // Scene 4: Gentle drift outward (back to start)
      float angle = time * moveSpeed * 0.5;
      float radius = fract(journey * 5.0) * 0.5;
      offset = vec2(sin(angle), cos(angle)) * radius;
    }
    
    // Apply depth to offset
    offset *= depth * moveSpeed;
    
    // Layer coordinates with parallax effect
    vec2 layerUV = uv * scale + offset;
    
    // Background pattern
    float intensity = journeyBackground(layerUV, i);
    
    // Dynamic colors based on scene
    float hue = journey + i * 0.1 + time * 0.01;
    float saturation = 0.7 + 0.3 * cos(time * 0.03 + i * 0.4);
    vec3 color = journeyPalette(intensity + i * 0.2, saturation, 1.0 + i * 0.1);
    
    // Layer weight - closer layers are more prominent
    float weight = 1.0 - i / layers;
    totalWeight += weight;
    
    // Add to result
    result += color * intensity * weight;
  }
  
  // Normalize by total weight
  return result / totalWeight;
}

// Neon lines effect with journey progression
vec3 journeyLines(vec2 uv, float reactivity) {
  // Get journey progress
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  
  // Scene-specific line parameters
  float thickness, brightness, speed;
  
  if (scene < 1.0) {
    // Scene 0: Thin, bright lines fading in
    thickness = 0.01 + reactivity * 0.1;
    brightness = mix(0.05, 0.2, fract(journey * 5.0)) + reactivity * 1.0;
    speed = 0.1 + reactivity * 0.1;
  } else if (scene < 2.0) {
    // Scene 1: Thicker, glowing lines
    thickness = 0.02 + reactivity * 0.15;
    brightness = 0.2 + reactivity * 1.2;
    speed = 0.15 + reactivity * 0.15;
  } else if (scene < 3.0) {
    // Scene 2: Maximum intensity
    thickness = 0.03 + reactivity * 0.2;
    brightness = 0.25 + reactivity * 1.5;
    speed = 0.2 + reactivity * 0.2;
  } else if (scene < 4.0) {
    // Scene 3: Pulsing with the beat
    thickness = 0.025 + reactivity * 0.15 + 0.01 * sin(time * 0.2);
    brightness = 0.2 + reactivity * 1.3 + 0.1 * sin(time * 0.3);
    speed = 0.18 + reactivity * 0.18;
  } else {
    // Scene 4: Fading to transition back
    thickness = mix(0.025, 0.01, fract(journey * 5.0)) + reactivity * 0.1;
    brightness = mix(0.2, 0.05, fract(journey * 5.0)) + reactivity * 1.0;
    speed = mix(0.18, 0.1, fract(journey * 5.0)) + reactivity * 0.1;
  }
  
  // Generate line pattern
  float lines = neonLines(uv, thickness, brightness, speed, time * 0.1);
  
  // Echo effect for lines
  for (float i = 1.0; i < MAX_ECHO_LAYERS; i++) {
    lines += neonLines(uv, thickness, brightness * 0.5, speed, treble * 0.1 - i * 0.5) * pow(0.6, i);
  }
  
  // Scene-specific line colors
  vec3 lineColor = journeyPalette(lines * 0.2 + bassDrive * 0.005, 0.8, 1.2);
  
  return lineColor * lines;
}

void main() {
  // Normalize coordinates
  vec2 uv = (gl_FragCoord.xy - 0.5 * resolution.xy) / min(resolution.x, resolution.y);
  
  // Journey progress
  float journey = getJourneyProgress();
  float scene = getSceneIndex();
  float transition = getSceneTransition();
  
  // Apply zoom with scene-specific variations
  float journeyZoom = zoom * 0.6;
  if (scene < 1.0) {
    // Scene 0: Zooming in slightly
    journeyZoom *= mix(0.9, 1.1, transition);
  } else if (scene < 2.0) {
    // Scene 1: Pulsing zoom
    journeyZoom *= 1.1 + 0.1 * sin(time * 0.2);
  } else if (scene < 3.0) {
    // Scene 2: Maximum zoom
    journeyZoom *= 1.2;
  } else if (scene < 4.0) {
    // Scene 3: Zooming out
    journeyZoom *= mix(1.2, 0.9, transition);
  } else {
    // Scene 4: Back to normal
    journeyZoom *= mix(0.9, 1.0, transition);
  }
  
  uv *= journeyZoom;
  
  // Smooth audio values
  float smoothBass = bass * 0.8;
  float smoothMid = mid * 0.7;
  float smoothTreble = treble * 0.6;
  float audioReactivity = (smoothBass + smoothMid + smoothTreble) * 0.2;
  
  // Apply journey-based kaleidoscope
  float segments = 4.0 + floor(smoothMid * 0.5);
  float intensity = 0.1 + 0.2 * sin(timeX * 0.1);
  vec2 kaleidoUV = journeyKaleido(uv, segments, intensity);
  
  // Blend between original and kaleidoscope coordinates with scene-specific blending
  float kaleidoBlend;
  if (scene < 1.0) {
    // Scene 0: Gradually introducing kaleidoscope
    kaleidoBlend = mix(0.2, 0.5, transition);
  } else if (scene < 2.0) {
    // Scene 1: Strong kaleidoscope
    kaleidoBlend = 0.7;
  } else if (scene < 3.0) {
    // Scene 2: Maximum kaleidoscope
    kaleidoBlend = 0.8;
  } else if (scene < 4.0) {
    // Scene 3: Slightly reducing
    kaleidoBlend = 0.6;
  } else {
    // Scene 4: Back to subtle for loop
    kaleidoBlend = mix(0.6, 0.2, transition);
  }
  
  vec2 finalUV = mix(uv, kaleidoUV, kaleidoBlend);
  
  // Generate parallax layers with journey progression
  vec3 patternColor = journeyParallax(finalUV, 3.0);
  
  // Generate neon line effect with journey progression
  vec3 lineColor = journeyLines(finalUV, audioReactivity);
  
  // Scene-specific blend between background and lines
  float lineWeight;
  if (scene < 1.0) {
    // Scene 0: Lines appearing from background
    lineWeight = mix(0.3, 0.6, transition);
  } else if (scene < 2.0) {
    // Scene 1: Strong lines
    lineWeight = 0.7;
  } else if (scene < 3.0) {
    // Scene 2: Maximum line prominence
    lineWeight = 0.8;
  } else if (scene < 4.0) {
    // Scene 3: More balanced
    lineWeight = 0.7;
  } else {
    // Scene 4: Return to starting values
    lineWeight = mix(0.7, 0.3, transition);
  }
  
  vec3 color = patternColor * (1.0 - lineWeight) + lineColor * lineWeight;
  
  // Add gentle vignette
  float vignette = 1.0 - dot(uv * 0.5, uv * 0.5);
  color *= vignette;
  
  // Scene-specific glow
  float sceneGlow;
  if (scene < 1.0) {
    sceneGlow = mix(0.2, 0.4, transition);
  } else if (scene < 2.0) {
    sceneGlow = 0.5;
  } else if (scene < 3.0) {
    sceneGlow = 0.6;
  } else if (scene < 4.0) {
    sceneGlow = 0.5;
  } else {
    sceneGlow = mix(0.5, 0.2, transition);
  }
  
  float glow = audioReactivity * glowIntensity * sceneGlow;
  color += color * glow * 0.3;
  
  // Scene-specific contrast
  float contrast;
  if (scene < 1.0) {
    contrast = mix(0.6, 0.55, transition);
  } else if (scene < 2.0) {
    contrast = 0.5;
  } else if (scene < 3.0) {
    contrast = 0.45; // More contrast
  } else if (scene < 4.0) {
    contrast = 0.5;
  } else {
    contrast = mix(0.5, 0.6, transition);
  }
  
  color = pow(color, vec3(contrast));
  
  // Final alpha with scene progression
  float baseAlpha;
  if (scene < 1.0) {
    baseAlpha = 0.4 + transition * 0.1;
  } else if (scene < 2.0) {
    baseAlpha = 0.5;
  } else if (scene < 3.0) {
    baseAlpha = 0.6;
  } else if (scene < 4.0) {
    baseAlpha = 0.5;
  } else {
    baseAlpha = mix(0.5, 0.4, transition);
  }
  
  float alpha = baseAlpha + 0.1 * sin(time * 0.2) + smoothTreble * 0.03;
  
  gl_FragColor = vec4(color, alpha);
}
`,

  // Add more shaders here...
  
  // Blend shaders
  dissolveBlend: `
    uniform float blendFactor;
    uniform sampler2D texture1;
    uniform sampler2D texture2;
    uniform vec2 resolution;
    uniform float timeI;
    uniform float time;
    uniform float bass;
    uniform float mid;
    uniform float treble;
    varying vec2 vUv;

    // Simplex 2D noise
    vec3 permute(vec3 x) { return mod(((x*34.0)+1.0)*x, 289.0); }

    float snoise(vec2 v) {
      const vec4 C = vec4(0.211324865405187, 0.366025403784439,
              -0.577350269189626, 0.024390243902439);
      vec2 i  = floor(v + dot(v, C.yy) );
      vec2 x0 = v -   i + dot(i, C.xx);
      vec2 i1;
      i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
      vec4 x12 = x0.xyxy + C.xxzz;
      x12.xy -= i1;
      i = mod(i, 289.0);
      vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
      + i.x + vec3(0.0, i1.x, 1.0 ));
      vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy),
        dot(x12.zw,x12.zw)), 0.0);
      m = m*m ;
      m = m*m ;
      vec3 x = 2.0 * fract(p * C.www) - 1.0;
      vec3 h = abs(x) - 0.5;
      vec3 ox = floor(x + 0.5);
      vec3 a0 = x - ox;
      m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
      vec3 g;
      g.x  = a0.x  * x0.x  + h.x  * x0.y;
      g.yz = a0.yz * x12.xz + h.yz * x12.yw;
      return 130.0 * dot(m, g);
    }

    // Improved smooth step function
    float smootherstep(float edge0, float edge1, float x) {
        x = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
        return x * x * x * (x * (x * 6.0 - 15.0) + 10.0);
    }

    void main() {
        vec2 uv = gl_FragCoord.xy / resolution.xy;
        vec4 color1 = texture2D(texture1, vUv);
        vec4 color2 = texture2D(texture2, vUv);

        // Create multiple layers of noise
        float n1 = snoise(uv * 3.0 + timeI * 0.2);
        float n2 = snoise(uv * 5.0 - timeI * 0.15);
        float n3 = snoise(uv * 8.0 + timeI * 0.1);

        // Combine noise layers
        float finalNoise = (n1 + n2 + n3) / 3.0;

        // Create smooth transition effect
        float transitionEdge = smootherstep(-0.2, 1.2, blendFactor + finalNoise * 0.4);

        // Audio reactive elements
        float audioInfluence = (bass + mid + treble) * 0.1;
        transitionEdge += audioInfluence * sin(uv.x * 30.0 + uv.y * 20.0 + time * 2.0);
        transitionEdge -= audioInfluence * cos(uv.y * 40.0 - uv.x * 30.0 - time * 1.5);
        transitionEdge += audioInfluence * sin(uv.x * 50.0 - uv.y * 50.0 + time * 3.0);

        // Ensure transition edge stays within [0, 1] range
        transitionEdge = clamp(transitionEdge, 0.0, 1.0);

        // Smooth blend between the two textures
        vec3 finalColor = mix(color1.rgb, color2.rgb, transitionEdge);

        gl_FragColor = vec4(finalColor, 1.0);
    }
  `,
  
advancedBlend: ` 
  uniform float blendFactor;
  uniform sampler2D texture1;
  uniform sampler2D texture2;
  uniform vec2 resolution;
  uniform float time;
  varying vec2 vUv;
  
  // Simple hash function
  float hash(vec2 p) {
      return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
  }
  
  // Function to blend colors
  vec3 blendColors(vec3 color1, vec3 color2, float factor) {
      return mix(color1, 1.0 - (1.0 - color1) * (1.0 - color2), factor);
  }
  
  // Improved easing function for smoother fade
  float easeInOutCubic(float x) {
      return x < 0.5 ? 4.0 * x * x * x : 1.0 - pow(-2.0 * x + 2.0, 3.0) / 2.0;
  }
  
  void main() {
      vec2 uv = gl_FragCoord.xy / resolution.xy;
      vec4 color1 = texture2D(texture1, vUv);
      vec4 color2 = texture2D(texture2, vUv);
  
      // Use improved easing for fades
      float easedBlend = easeInOutCubic(blendFactor);
      float fade1 = smoothstep(1.0, 0.0, easedBlend * 0.8); // Fade out slightly faster
      float fade2 = smoothstep(0.5, 1.0, easedBlend * 1.5); // Fade in slightly slower
  
      // Enhanced ripple effect
      vec2 center = vec2(0.5, 0.5);
      float dist = length(uv - center);
      float rippleSpeed = 5.0; // Adjust for faster/slower ripples
      float rippleFreq = 15.0; // Adjust for more/fewer ripples
      float rippleAmp = 0.8; // Adjust for stronger/weaker ripples
      float ripple = sin(dist * rippleFreq - time * rippleSpeed) * rippleAmp;
      ripple *= smoothstep(0.7, 0.0, dist); // Fade out towards edges
      ripple *= smoothstep(0.0, 0.2, blendFactor) * smoothstep(1.0, 0.8, blendFactor); // Only during transition
  
      // Blend colors for the ripple effect
      vec3 rippleColor = mix(color1.rgb, color2.rgb, easedBlend + ripple * 0.6);
  
      // Enhanced noise dissolution
      float noise = hash(uv * 1.5 + time * 0.5);
      float noiseEffect = smoothstep(0.2, 0.7, mix(noise, 0.5, easedBlend));
  
      // Combine effects with smoother mixing
      float finalBlend = mix(easedBlend, noiseEffect, smoothstep(0.2, 0.8, easedBlend));
      finalBlend = mix(finalBlend, finalBlend + ripple, 0.5); // Increased ripple influence
  
      // Ensure color1 is fully faded out by the end of the transition
      color1 *= fade1;
      color2 *= fade2;
  
      // Final color mix with improved blending and ripple effect
      vec3 finalColor = mix(color1.rgb, color2.rgb, smoothstep(0.0, 1.0, finalBlend));
      finalColor = mix(finalColor, rippleColor, abs(ripple) * 0.8); // Blend in the ripple color
  
      gl_FragColor = vec4(finalColor, 1.0);
  }
  `,
  
  // Add more blend shaders here...
};



// Let the HTML know we're ready
console.log("Shaders loaded! 🎉");
