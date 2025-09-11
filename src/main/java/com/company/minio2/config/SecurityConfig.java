package com.company.minio2.config;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
public class SecurityConfig {

    // CHAIN CHỈ ÁP DỤNG CHO /api/**
    @Bean
    @Order(1) // ưu tiên chain này trước chain mặc định của Jmix
    public SecurityFilterChain apiChain(HttpSecurity http) throws Exception {
        http
                // Chỉ match các URL /api/**
                .securityMatcher("/api/**")
                .csrf(csrf -> csrf.disable())

                // Stateless cho API
                .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))

                // Phân quyền cho API
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(HttpMethod.POST, "/api/minio/upload").permitAll() // mở để test
                        // .requestMatchers("/api/**").authenticated() // khi muốn bảo mật
                        .anyRequest().permitAll()
                )

                // (tuỳ chọn) Basic Auth nếu bạn muốn bật authenticated() ở trên
                .httpBasic(Customizer.withDefaults());

        return http.build();
    }
}
